# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Process the RAW ROV data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/rov_utils.R")

## Transmittance data

files <- list.files("data/raw/Katlein-etal_2016/datasets/", "rovT_irrad", full.names = TRUE)

transmittance <- map(files, read_transmittance) %>% 
  bind_rows()

## Irradiance data

files <- list.files("data/raw/Katlein-etal_2016/datasets/", "rov_irrad", full.names = TRUE)

irradiance <- map(files, read_irradiance) %>% 
  bind_rows()

## Depth data (for some reasons, the distance to ice bottom are in other files)

files <- list.files("data/raw/Katlein-etal_2016/datasets/", "positioning", full.names = TRUE)

depth <- map(files, read_depth) %>% 
  bind_rows()

# depth %>%
#   select(-(dist_rel_x_m:dist_sea_ice_bottom_m), -filename, -station, -head_deg) %>%
#   gather(angle, degree, -date_time) %>%
#   ggplot(aes(x = date_time, y = degree)) +
#   geom_line(aes(color = angle)) +
#   geom_point(aes(color = angle), size = 0.1) +
#   geom_hline(aes(yintercept = 10), lty = 2) +
#   geom_hline(aes(yintercept = -10), lty = 2) +
#   facet_wrap(~format(date_time, "%d"), scales = "free")
# 
# depth %>% 
#   ggplot(aes(x = date_time, y = head_deg))+
#   geom_point() +
#   facet_wrap(~format(date_time, "%d"), scales = "free")

## Following a discussion with Christian, we only keep data with pitch and roll
## between -10 and 10 degrees.

depth <- depth %>% 
  filter(pitch_deg >= -10 & pitch_deg <= 10) %>% 
  filter(roll_deg >= -10 & roll_deg <= 10)

## Interpolate depth to ice bottom

transmittance <- transmittance %>% 
  group_by(filename) %>% 
  nest() %>% 
  mutate(dist_sea_ice_bottom_m = map(data, interpol_depth, depth = depth)) %>% 
  unnest(data, dist_sea_ice_bottom_m) %>% 
  mutate(ice_thickness = depth_water_m - dist_sea_ice_bottom_m) %>% 
  filter(ice_thickness >= 0)

irradiance <- irradiance %>% 
  group_by(filename) %>% 
  nest() %>% 
  mutate(dist_sea_ice_bottom_m = map(data, interpol_depth, depth = depth)) %>% 
  unnest(data, dist_sea_ice_bottom_m) %>% 
  mutate(ice_thickness = depth_water_m - dist_sea_ice_bottom_m) %>%
  filter(ice_thickness >= 0)

write_feather(transmittance, "data/clean/rov_transmittance.feather")
write_feather(irradiance, "data/clean/rov_irradiance.feather")

# Plot the data -----------------------------------------------------------

p1 <- transmittance %>% 
  ggplot(aes(x = dist_rel_x_m, y = dist_rel_y_m)) +
  geom_point(size = 0.1) +
  facet_wrap(~station, scales = "free")

p2 <- transmittance %>% 
  ggplot(aes(x = transmittance_percent)) +
  geom_histogram() +
  facet_wrap(~station, scales = "free") +
  scale_x_log10() +
  annotation_logticks(sides = "b")

ggsave("graphs/rov_transmittance_position.pdf", p1)
ggsave("graphs/rov_transmittance_histogram.pdf", p2)
ggsave("graphs/png/rov_transmittance_position.png", p1)
ggsave("graphs/png/rov_transmittance_histogram.png", p2)
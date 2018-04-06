# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Process the RAW ROV data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/rov/rov_utils.R")

## Transmittance data

files <- list.files("data/raw/Katlein-etal_2016/datasets/", "rovT_irrad", full.names = TRUE)

transmittance <- map(files, read_transmittance) %>%
  bind_rows() %>%
  distinct()

## Irradiance data

files <- list.files("data/raw/Katlein-etal_2016/datasets/", "rov_irrad", full.names = TRUE)

irradiance <- map(files, read_irradiance) %>%
  bind_rows() %>%
  distinct()

## Depth data (for some reasons, the distance to ice bottom are in other files)

files <- list.files("data/raw/Katlein-etal_2016/datasets/", "positioning", full.names = TRUE)

depth <- map(files, read_depth) %>%
  bind_rows()

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
  filter(ice_thickness >= 0) %>%
  separate(station, into = c("station", "cast"), convert = TRUE)

irradiance <- irradiance %>%
  group_by(filename) %>%
  nest() %>%
  mutate(dist_sea_ice_bottom_m = map(data, interpol_depth, depth = depth)) %>%
  unnest(data, dist_sea_ice_bottom_m) %>%
  mutate(ice_thickness = depth_water_m - dist_sea_ice_bottom_m) %>%
  filter(ice_thickness >= 0) %>%
  separate(station, into = c("station", "cast"), convert = TRUE)

irradiance %>%
  distinct(station, cast, date)

write_feather(transmittance, "data/clean/rov_transmittance.feather")
write_feather(irradiance, "data/clean/rov_irradiance.feather")

# Plot the data -----------------------------------------------------------

p1 <- transmittance %>%
  ggplot(aes(x = dist_rel_x_m, y = dist_rel_y_m, color = factor(date))) +
  geom_point(size = 0.1) +
  facet_wrap(~ station + date, scales = "free") +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  labs(color = "Date")

p2 <- transmittance %>%
  ggplot(aes(x = transmittance)) +
  geom_histogram() +
  facet_wrap(~ station, scales = "free") +
  scale_x_log10(label = scales::percent) +
  annotation_logticks(sides = "b") +
  labs(title = "Histograms of transmittance measured by the ROV") +
  labs(subtitle = sprintf("Total of %d measurements", nrow(transmittance)))

ggsave("graphs/rov_transmittance_position.pdf", p1)
ggsave("graphs/rov_transmittance_histogram.pdf", p2)

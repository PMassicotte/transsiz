# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Process the RAW ROV data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/rov/rov_utils.R")

ncores <- detectCores() - 1

## Transmittance data

files <- list.files("data/raw/Katlein-etal_2016/datasets/", "rovT_irrad", full.names = TRUE)

transmittance <- mclapply(files, read_transmittance, mc.cores = ncores) %>%
  bind_rows() %>%
  distinct()

## Irradiance data

files <- list.files("data/raw/Katlein-etal_2016/datasets/", "rov_irrad", full.names = TRUE)

irradiance <- mclapply(files, read_irradiance, mc.cores = ncores) %>%
  bind_rows() %>%
  distinct()


# Distance from the ice bottom --------------------------------------------

## Depth data (for some reasons, the distance to ice bottom are in other files)

files <- list.files("data/raw/Katlein-etal_2016/datasets/", "positioning", full.names = TRUE)

depth <- map(files, read_depth) %>%
  bind_rows()

## Data clean up
depth <- depth %>%
  filter(pitch_deg >= -10 & pitch_deg <= 10) %>%
  filter(roll_deg >= -10 & roll_deg <= 10)

## Looks like there are some important differences between dist_sea_ice_bottom_m
## and depth_water_m measures.

# depth %>%
#   ggplot(aes(x = date_time)) +
#   geom_line(aes( y = dist_sea_ice_bottom_m, color = "dist_sea_ice_bottom_m")) +
#   geom_line(aes(y = depth_water_m, color = "depth_water_m")) +
#   facet_wrap(~station, scales = "free")

## Interpolate depth to ice bottom. This is done because the time stamp of
## dist_sea_ice_bottom_m measurements do not match those of the ROV
## transmittance data.

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

ggsave("graphs/rov_transmittance_position.pdf", p1)

p2 <- transmittance %>%
  ggplot(aes(x = transmittance)) +
  geom_histogram() +
  facet_wrap(~ station) +
  scale_x_continuous(labels = scales::percent) +
  annotation_logticks(sides = "b") +
  labs(title = "Histograms of transmittance measured by the ROV") +
  labs(subtitle = sprintf("Total of %d measurements", nrow(transmittance)))

p3 <- transmittance %>%
  ggplot(aes(x = date_time, y = depth_water_m)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ station, scales = "free") +
  labs(title = "Depth over the time") +
  scale_y_reverse()

p <- cowplot::plot_grid(p2, p3, ncol = 1, align = "hv", labels = "AUTO")
ggsave("graphs/rov_transmittance_histogram.pdf", device = cairo_pdf, height = 12, width = 10)

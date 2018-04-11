# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate mean transmittance at each station.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

## Read raw transmittance data

source("R/zzz.R")

circle <- function(center = c(0, 0), diameter = 1, npoints = 100) {
  r <- diameter / 2
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)

  return(data_frame(x = xx, y = yy))
}

files <- list.files("data/raw/Katlein-etal_2016/datasets/", "rovT_irrad", full.names = TRUE)

trans <- lapply(files, read_transmittance) %>%
  set_names(basename(files)) %>%
  bind_rows(.id = "station") %>%
  mutate(station = stringr::str_sub(station, 1, 10)) %>%
  mutate(transmittance_percent = transmittance_percent * 1.3) ## See email from Christian

trans %>%
  distinct(station, format(date_time, "%Y-%m-%d"))

## Transmittance histo

p <- trans %>%
  ggplot(aes(x = transmittance_percent)) +
  geom_histogram(binwidth = 0.1) +
  facet_wrap(~ station, scales = "free_y") +
  scale_x_log10() +
  annotation_logticks(sides = "b") +
  xlab("log(transmittance)")

ggsave("graphs/histo_transmittance.pdf", width = 9, height = 7)

## Depth histo

p <- trans %>%
  ggplot(aes(x = depth_water_m)) +
  geom_histogram(binwidth = 0.50) +
  facet_wrap(~ station, scales = "free")

ggsave("graphs/histo_transmittance_depth.pdf", width = 9, height = 7)

## Calculate the distance of each points from the 0,0 cordinate

r <- 3 # maximum radius

trans <- trans %>%
  mutate(dist = sqrt(dist_rel_x_m^2 + dist_rel_y_m^2)) %>%
  filter(dist >= r) %>%
  filter(depth_water_m >= 0.7 & depth_water_m <= 3) # Only keep obs. between these depths

## Plot ROV position with data used to calculate mean transmittance

dat <- circle(c(0, 0), r, npoints = 100)

p <- trans %>%
  ggplot(aes(x = dist_rel_x_m, y = dist_rel_y_m)) +
  geom_point(aes(color = transmittance_percent), size = 0.3) +
  facet_wrap(~ station, scales = "free") +
  geom_path(data = dat, aes(x, y), color = "red", lwd = 0.1) +
  xlab("Relative distance in X (meters)") +
  ylab("Relative distance in Y (meters)") +
  labs(
    color = "Transmittance (%)",
    title = sprintf("Red circle has a radius of %2.2f meters around c(0, 0)", r)
  ) +
  viridis::scale_color_viridis()

## Calculate mean transmittance

res <- trans %>%
  group_by(station) %>%
  summarise(mean_transmittance = mean(transmittance_percent, na.rm = TRUE), n = n())

write_feather(res, "data/clean/mean_transmittance.feather")

## Add these values to the panels

p <- p +
  geom_text(
    data = res,
    aes(
      x = -Inf,
      y = Inf,
      label = sprintf("Mean transmittance: %2.2f%%", mean_transmittance)
    ),
    size = 2,
    vjust = 2,
    hjust = -0.1
  )

ggsave("graphs/transmittance_map.pdf", height = 7, width = 10)

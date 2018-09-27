rm(list = ls())

kd <- read_csv("data/clean/rov_kd.csv") %>%
  select(station, cast, term, estimate) %>%
  spread(term, estimate)

transmittance <- read_feather("data/clean/rov_transmittance.feather") %>%
  inner_join(kd, by = c("station", "cast")) %>%
  filter(dist_sea_ice_bottom_m <= 3) ## The model is not good at predicting at higher depth, paper from Christian

  
## Predict transmittance just bellow ice
transmittance <- transmittance %>%
  mutate(transmittance_ed0 = transmittance / (exp(-kd * (0 - dist_sea_ice_bottom_m))))

transmittance %>%
  ggplot(aes(x = transmittance_ed0)) +
  geom_histogram() +
  facet_wrap(~ station + cast, scales = "free_y") +
  scale_x_log10(labels = function(x) sprintf("%2.2f%%", x * 100)) +
  annotation_logticks(sides = "b")

## Propagate PAR value in the water column

par <- read_csv("data/clean/pyranometer.csv")

transmittance <- transmittance %>%
  left_join(par, by = c("station", "cast"))

## For each ROV measurement, propagate light between 0 and 100 m

pred_light <- function(df) {
  depth <- 0:40
  par_z_variable_transmittance <- with(df, par_just_below_surface_µmol * exp(-kd * depth) * transmittance_ed0)
  par_z_100_percent_transmittance <- with(df, par_just_below_surface_µmol * exp(-kd * depth) * 1)
  
  return(data.frame(depth, par_z_variable_transmittance, par_z_100_percent_transmittance))
}

cl <- create_cluster(detectCores() - 1)
cluster_copy(cl, pred_light)

transmittance <- transmittance %>%
  mutate(id = 1:nrow(.))

res <- transmittance %>%
  partition(id, cluster = cl) %>%
  do(xx = pred_light(.)) %>%
  collect() %>%
  unnest() %>%
  arrange(id, depth) %>%
  ungroup()

## Select only important variables
res <- res <- inner_join(res, transmittance) %>%
  select(
    id,
    filename,
    date_time,
    station,
    cast,
    transmittance,
    par_just_below_surface_µmol,
    depth,
    hour,
    par_z_variable_transmittance,
    par_z_100_percent_transmittance,
    transmittance_ed0
  )

# Have a look to some data ------------------------------------------------

p <- res %>%
  filter(station == 19) %>%
  ggplot(aes(x = par_z_variable_transmittance, y = depth, group = id)) +
  geom_path(size = 0.15, alpha = 0.5) +
  scale_y_reverse() +
  facet_wrap(~ hour) +
  labs(title = "Pyranometer PAR propagated in water column between 0-15 meters using ROV transmittance (station 19)") +
  labs(
    subtitle = str_wrap(
      "Each box is an hour of the day and each line correspond to a ROV measurement. For this particular station, we had 1 561 ROV measurments. Thus each box (hour) contains 1 561 PAR profiles.",
      width = 120
    )
  )

ggsave("graphs/rov_propagated_par.pdf", device = cairo_pdf, height = 10, width = 12)

# Export ------------------------------------------------------------------

write_feather(res, "data/clean/rov_propagated_par_water_column.feather")

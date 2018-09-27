rm(list = ls())

kd <- read_csv("data/clean/rov_kd.csv") %>%
  select(station, term, estimate) %>%
  spread(term, estimate)

transmittance <- read_feather("data/clean/suit_transmittance.feather") %>%
  inner_join(kd, by = c("station"))

## Depth of measurement (draft) was assumed to reflect the transmittance just
## below ice (i.e. transmittance_ed0)
transmittance <- transmittance %>%
  rename(transmittance_ed0 = transmittance)

transmittance %>%
  ggplot(aes(x = transmittance_ed0)) +
  geom_histogram() +
  facet_wrap(~ station, scales = "free_y") +
  scale_x_log10(labels = function(x) sprintf("%2.2f%%", x * 100)) +
  annotation_logticks(sides = "b")

## Propagate PAR value in the water column

par <- read_csv("data/clean/pyranometer.csv")

transmittance <- transmittance %>%
  left_join(par, by = "station")

## For each SUIT measurement, propagate light between 0 and 100 m

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
    date_time,
    station,
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
  labs(title = "Pyranometer PAR propagated in water column between 0-15 meters using SUIT transmittance (station 19)") +
  labs(
    subtitle = str_wrap(
      "Each box is an hour of the day and each line correspond to a ROV measurement. For this particular station, we had 239 SUIT measurments. Thus each box (hour) contains 239 PAR profiles.",
      width = 120
    )
  )

ggsave("graphs/suit_propagated_par.pdf", device = cairo_pdf, height = 10, width = 12)

# Export ------------------------------------------------------------------

write_feather(res, "data/clean/suit_propagated_par_water_column.feather")

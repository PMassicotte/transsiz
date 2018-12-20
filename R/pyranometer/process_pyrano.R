rm(list = ls())

## Match the pyrano data using the ROV dates
stations <- read_csv("data/clean/stations.csv")

pyrano <- data.table::fread("data/raw/PS92_cont_surf_Pyrano.txt") %>%
  setNames(iconv(names(.), "latin1", "utf-8", sub = "byte")) %>%
  janitor::clean_names(case = "old_janitor") %>%
  as_tibble() %>%
  select(c(1, 2, 3, 7)) %>%
  mutate(date_time = anytime::anytime(date_time)) %>%
  mutate(date = lubridate::date(date_time)) %>%
  mutate(date_numeric = lubridate::hour(date_time) * 3600 + lubridate::minute(date_time) * 60 + lubridate::second(date_time)) %>%
  mutate(hour = lubridate::hour(date_time)) %>%
  mutate(minute = lubridate::minute(date_time)) %>%
  filter(par_just_below_surface_µmol >= 0)

## Keep only pyrano data matching the ice stations
pyrano <- pyrano %>%
  inner_join(stations, by = "date") %>%
  separate(station, into = c("station", "cast"), convert = TRUE)

pyrano %>%
  distinct(station, cast, date)

# There is a problem with data later than 2015-06-20 20:20:00. Replace these
# "outliers" with the observations measured at the begining.

# # 2015-06-11
# i <- which(pyrano$date_time >= "2015-06-11 20:00:00" & pyrano$date == "2015-06-11")
# pyrano$par_just_below_surface_µmol[i] <- pyrano$par_just_below_surface_µmol[pyrano$date == "2015-06-11"][length(i):1]
# # pyrano$par_just_below_surface_µmol_ice[i] <- pyrano$par_just_below_surface_µmol_ice[pyrano$date == "2015-06-11"][length(i):1]
#
# # # 2015-06-20
i <- which(pyrano$date_time >= "2015-06-20 20:20:00" & pyrano$date == "2015-06-20")
pyrano$par_just_below_surface_µmol[i] <- pyrano$par_just_below_surface_µmol[pyrano$date == "2015-06-20"][length(i):1]
# # # pyrano$par_just_below_surface_µmol_ice[i] <- pyrano$par_just_below_surface_µmol_ice[pyrano$date == "2015-06-20"][length(i):1]

p <- pyrano %>%
  ggplot(aes(x = date_time, y = par_just_below_surface_µmol)) +
  geom_line() +
  facet_wrap(~ date + paste(station, cast, sep = "-"), scales = "free", ncol = 4) +
  scale_x_datetime(
    date_labels = "%H:%M:%S",
    expand = c(0.2, 0),
    breaks = scales::pretty_breaks(n = 4)
  ) +
  labs(
    title = "PAR measured by the pyrano",
    subtitle = "This is the PAR just bellow surface measured every 5 min. Does not take into account ice transmittance."
  )

ggsave("graphs/pyrano_5min_par.pdf", width = 10, height = 5)

# Hourly PAR --------------------------------------------------------------

pyrano <- pyrano %>%
  group_by(station, cast, date, hour) %>%
  nest() %>%
  mutate(par_just_below_surface_µmol = map(data, ~ mean(.$par_just_below_surface_µmol))) %>%
  unnest(par_just_below_surface_µmol)

p <- pyrano %>%
  ggplot(aes(x = hour, y = par_just_below_surface_µmol)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ date + paste(station, cast, sep = "-"), scales = "free", ncol = 4) +
  xlab("Time (hour)") +
  labs(
    title = "PAR measured by the pyrano (averaged by hour)",
    subtitle = "This is the PAR just bellow surface. Does not take into account ice transmittance."
  )

ggsave("graphs/pyrano_hourly_par.pdf", width = 10, height = 5)

pyrano %>%
  select(-data) %>%
  write_csv("data/clean/pyranometer.csv")


# Conversion factor -------------------------------------------------------

pyrano <- data.table::fread("data/raw/PS92_cont_surf_Pyrano.txt") %>%
  setNames(iconv(names(.), "latin1", "utf-8", sub = "byte")) %>%
  janitor::clean_names(case = "old_janitor") %>%
  as_tibble()

## It seems that a factor of 4.49 was used to convert from wm2 to umol
pyrano %>% 
  filter(par_down_above_surface_w_m2 > 0) %>% 
  mutate(test = par_just_below_surface_µmol / par_just_below_surf_w_m2)


pyrano %>% 
  ggplot(aes(x = longitude_e, y = latitude_n)) +
  geom_path() +
  ylim(c(81, NA))

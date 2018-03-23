# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Pyranometer data from Christian.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

## "Official" stations
stations <- readxl::read_excel("data/raw/Sampling_Takuvik.xlsx", skip = 1) %>% 
  janitor::clean_names(case = "old_janitor") %>% 
  mutate(date = as.Date(date)) %>% 
  filter(p_vs_e == "x") %>% 
  select(station, date) %>% 
  mutate(station = gsub("/", "_", station))

group <- readxl::read_excel("data/raw/Sampling_Takuvik.xlsx", sheet = 2) %>% 
  mutate(date = as.Date(date))

# *************************************************************************
# Read pyranometer data
# *************************************************************************

pyrano <- data.table::fread("data/raw/PS92_cont_surf_Pyrano.txt") %>% 
  setNames(iconv(names(.), "latin1", "utf-8", sub = "byte")) %>% 
  janitor::clean_names(case = "old_janitor") %>% 
  as_tibble() %>% 
  select(c(1,2,3, 7)) %>% 
  # mutate(par_just_below_surf_w_m2_ice = par_down_above_surface_w_m2 * 0.04 * 1.66) %>% 
  # mutate(par_just_below_surface_µmol_ice  = par_just_below_surf_w_m2_ice * 4.59) %>% 
  # select(date_time, par_just_below_surface_µmol) %>% 
  mutate(date_time = anytime::anytime(date_time)) %>% 
  mutate(date = lubridate::date(date_time)) %>% 
  mutate(date_numeric = lubridate::hour(date_time) * 3600 + lubridate::minute(date_time) * 60 + lubridate::second(date_time)) %>% 
  mutate(hour = lubridate::hour(date_time)) %>% 
  mutate(minute = lubridate::minute(date_time)) %>% 
  filter(par_just_below_surface_µmol >= 0) %>% 
  inner_join(group, by = "date") # Keep only obs. matching the "official" list of stations

# There is a problem with data later than 2015-06-20 20:20:00. Replace these
# "outliers" with the observations measured at the begining.

# 2015-06-11
i <- which(pyrano$date_time >= "2015-06-11 20:00:00" & pyrano$date == "2015-06-11")
pyrano$par_just_below_surface_µmol[i] <- pyrano$par_just_below_surface_µmol[pyrano$date == "2015-06-11"][length(i):1]
# pyrano$par_just_below_surface_µmol_ice[i] <- pyrano$par_just_below_surface_µmol_ice[pyrano$date == "2015-06-11"][length(i):1]

# 2015-06-20
i <- which(pyrano$date_time >= "2015-06-20 20:20:00" & pyrano$date == "2015-06-20")
pyrano$par_just_below_surface_µmol[i] <- pyrano$par_just_below_surface_µmol[pyrano$date == "2015-06-20"][length(i):1]
# pyrano$par_just_below_surface_µmol_ice[i] <- pyrano$par_just_below_surface_µmol_ice[pyrano$date == "2015-06-20"][length(i):1]

# ************************************************************************** 
# Systematically average pyrano data for consecutive days of a same station
# sampling. See sheet "Sheet2" in Sampling_Takuvik.xlsx for grouping days.
# *************************************************************************

pyrano <- pyrano %>% 
  group_by(hour, minute, group) %>% 
  summarise(par_just_below_surface_µmol = mean(par_just_below_surface_µmol))

pyrano <- pyrano %>% 
  left_join(group, by = "group") %>% 
  mutate(date_time = lubridate::make_datetime(
    lubridate::year(date),
    lubridate::month(date),
    lubridate::day(date),
    hour,
    minute
  ))

p <- pyrano %>% 
  ggplot(aes(x = date_time, y = par_just_below_surface_µmol)) +
  geom_line(aes(color = group)) +
  facet_wrap(~date, scales = "free", ncol = 4) +
  scale_x_datetime(date_labels = "%H:%M:%S", expand = c(0.2, 0), breaks = scales::pretty_breaks(n = 4)) +
  xlab("Time (hour)") +
  ylab("PAR just above surface (umol s-1 m-2)") +
  labs(title = "This is the data from the pyranometer used to propagate light in the water column")

ggsave("graphs/pyrano.pdf", width = 10, height = 8)

# **************************************************************************** 
# Associate calculated transmittance to the stations. If transmittance is not 
# calculated (because open water), then assume 96% (A. Morel).
# ****************************************************************************

transmittance <- read_feather("data/clean/mean_transmittance.feather") %>% 
  separate(station, into = c("cruise", "station", "operation")) %>% 
  select(-cruise, -operation)

pyrano <- pyrano %>% 
  separate(station, into = c("cruise", "station", "operation"))  %>% 
  select(-cruise, -operation)

light <- pyrano %>% 
  left_join(transmittance, by = "station") %>% 
  drop_na(mean_transmittance) %>% 
  mutate(par_just_below_surface_µmol = par_just_below_surface_µmol * (mean_transmittance / 100))

hourly_par <- light %>% 
  group_by(date, hour) %>% 
  nest() %>% 
  mutate(e = map(data, ~mean(.$par_just_below_surface_µmol))) %>%
  unnest(e)

p <- hourly_par %>% 
  ggplot(aes(x = hour, y = e)) +
  geom_line() +
  geom_point() +
  facet_wrap(~date, scales = "free", ncol = 4) +
  xlab("Time (hour)") +
  ylab("Hourly under ice PAR (umol s-1 m-2)") +
  labs(title = "Hourly under ice PAR (corrected for transmittance)")

ggsave("graphs/hourly_par.pdf", width = 10, height = 8)

## Save hourly PAR

hourly_par %>% 
  select(-data) %>% 
  write_feather("data/clean/hourly_par.feather")


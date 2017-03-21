# *************************************************************************
# Read pyranometer data
# *************************************************************************

rm(list = ls())

pyrano <- data.table::fread("data/raw/PS92_cont_surf_Pyrano.txt") %>% 
  setNames(iconv(names(.), "latin1", "utf-8", sub = "byte")) %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  mutate(par_just_below_surf_w_m2_ice = par_down_above_surface_w_m2 * 0.04 * 1.66) %>% 
  mutate(par_just_below_surface_µmol_ice  = par_just_below_surf_w_m2_ice * 4.59) %>% 
  # select(date_time, par_just_below_surface_µmol) %>% 
  mutate(date_time = anytime::anytime(date_time)) %>% 
  mutate(date = lubridate::date(date_time)) %>% 
  mutate(date_numeric = lubridate::hour(date_time) * 3600 + lubridate::minute(date_time) * 60 + lubridate::second(date_time)) %>% 
  mutate(hour = lubridate::hour(date_time)) %>% 
  mutate(minute = lubridate::minute(date_time)) %>% 
  filter(par_just_below_surface_µmol >= 0) %>% 
  filter(date %in% as.Date(c(
    "2015-05-31",
    "2015-06-03",
    "2015-06-06",
    "2015-06-11",
    "2015-06-15",
    "2015-06-17",
    "2015-06-19",
    "2015-06-20"
  )))

# There is a problem with data later than 2015-06-20 20:20:00. Replace these
# "outliers" with the observations measured at the begining.

# 2015-06-11
i <- which(pyrano$date_time >= "2015-06-11 20:00:00" & pyrano$date == "2015-06-11")
pyrano$par_just_below_surface_µmol[i] <- pyrano$par_just_below_surface_µmol[pyrano$date == "2015-06-11"][length(i):1]
pyrano$par_just_below_surface_µmol_ice[i] <- pyrano$par_just_below_surface_µmol_ice[pyrano$date == "2015-06-11"][length(i):1]

# 2015-06-20
i <- which(pyrano$date_time >= "2015-06-20 20:20:00" & pyrano$date == "2015-06-20")
pyrano$par_just_below_surface_µmol[i] <- pyrano$par_just_below_surface_µmol[pyrano$date == "2015-06-20"][length(i):1]
pyrano$par_just_below_surface_µmol_ice[i] <- pyrano$par_just_below_surface_µmol_ice[pyrano$date == "2015-06-20"][length(i):1]

# ************************************************************************** 
# Average data of 2015-06-19 and 2015-06-20 because sampling hsa been done 
# during the night. Here I am using the mean of these two dates as the values of
# the 2015-06-19
# *************************************************************************

pyrano <- pyrano %>%
  mutate(date2 = if_else(date == "2015-06-20", date - lubridate::days(1), date)) %>%
  mutate(date_time2 = if_else(date == "2015-06-20", date_time - lubridate::days(1), date_time)) %>%
  select(-date, -date_time) %>% 
  rename(date = date2, date_time = date_time2) %>% 
  group_by(date_time, date, date_numeric, hour, minute) %>% 
  summarise(par_just_below_surface_µmol = mean(par_just_below_surface_µmol),
            par_just_below_surface_µmol_ice  = mean(par_just_below_surface_µmol_ice))

p <- pyrano %>% 
  ggplot(aes(x = date_time, y = par_just_below_surface_µmol)) +
  geom_line() +
  facet_wrap(~date, scales = "free") +
  scale_x_datetime(date_labels = "%H:%M:%S", expand = c(0.1, 0)) +
  xlab("Time (hour)") +
  ylab("PAR just below surface (umol s-1 m-2)") +
  labs(title = "This is the data from the pyranometer used to propagate light in the water column")

ggsave("graphs/pyrano.pdf", width = 12, height = 9)

hourly_par <- pyrano %>% 
  group_by(date, hour) %>% 
  nest() %>% 
  mutate(e = map(data, ~mean(.$par_just_below_surface_µmol))) %>%
  mutate(e_ice = map(data, ~mean(.$par_just_below_surface_µmol_ice))) %>%
  unnest(e, e_ice)

p <- hourly_par %>% 
  ggplot(aes(x = hour, y = e)) +
  geom_line() +
  geom_point() +
  facet_wrap(~date, scales = "free") +
  xlab("Time (hour)") +
  ylab("Hourly averaged PAR just below surface (umol s-1 m-2)") 

ggsave("graphs/hourly_par.pdf", width = 12, height = 9)

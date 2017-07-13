pyrano <- data.table::fread("data/raw/PS92_cont_surf_Pyrano.txt") %>% 
  setNames(iconv(names(.), "latin1", "utf-8", sub = "byte")) %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  select(c(1,2,3, 7)) %>% 
  mutate(date_time = anytime::anytime(date_time)) %>% 
  mutate(date = lubridate::date(date_time)) %>% 
  mutate(date_numeric = lubridate::hour(date_time) * 3600 + lubridate::minute(date_time) * 60 + lubridate::second(date_time)) %>% 
  mutate(hour = lubridate::hour(date_time)) %>% 
  mutate(minute = lubridate::minute(date_time)) %>% 
  filter(par_just_below_surface_µmol >= 0)

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

pyrano %>% 
  ggplot(aes(x = date_time, y =  par_just_below_surface_µmol)) +
  geom_line() +
  facet_wrap(~date, scales = "free") +
  scale_x_datetime(date_labels = "%H:%M:%S", expand = c(0.2, 0), breaks = scales::pretty_breaks(n = 4))

write_feather(pyrano, "data/clean/pyranometer.feather")

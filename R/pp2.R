# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  This is the script to process the second batch of samples. It 
#               is based on pp.R, but for more than 1 station.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/calculate_kd.R")

kd_par <- read_csv("data/kd_par.csv") %>% 
  dplyr::select(-station_sample)

# *************************************************************************
# Read pyranometer data
# *************************************************************************

pyrano <- data.table::fread("data/PS92_cont_surf_Pyrano.txt") %>% 
  setNames(iconv(names(.), "latin1", "utf-8", sub = "byte")) %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  select(date_time, par_just_below_surface_µmol) %>% 
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

# 2015-06-20
i <- which(pyrano$date_time >= "2015-06-20 20:20:00" & pyrano$date == "2015-06-20")
pyrano$par_just_below_surface_µmol[i] <- pyrano$par_just_below_surface_µmol[pyrano$date == "2015-06-20"][length(i):1]

# ************************************************************************* 
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
  summarise(par_just_below_surface_µmol = mean(par_just_below_surface_µmol))

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
  unnest(e)

p <- hourly_par %>% 
  ggplot(aes(x = hour, y = e)) +
  geom_line() +
  geom_point() +
  facet_wrap(~date, scales = "free") +
  xlab("Time (hour)") +
  ylab("Hourly averaged PAR just below surface (umol s-1 m-2)") 

ggsave("graphs/hourly_par.pdf", width = 12, height = 9)

# Primary production data -------------------------------------------------

pp <- read_csv("data/Data_PP_AllStations.csv") %>% 
  fill(date, depth) %>% 
  janitor::clean_names() %>% 
  drop_na() %>% 
  rename(light = light_incub) %>% 
  mutate(light = if_else(light < 0, 0, light)) %>% 
  mutate(date = lubridate::parse_date_time(date, order = "dmY")) %>% 
  mutate(date = lubridate::date(date)) %>% 
  # group_by(date, depth, light) %>%
  # summarise(p_manip = mean(p_manip, na.rm = TRUE)) %>% 
  # ungroup() %>% 
  group_by(date, depth) %>% 
  mutate(model_type = if_else(p_manip[which.max(light)] < max(p_manip), "model1", "model2"))

# pp %>% 
#   ggplot(aes(x = light, y = p_manip)) +
#   geom_point(aes(color = model_type)) +
#   facet_wrap(~gi, scales = "free")

fit_pe <- function(df, model_type) {
  
  mod <- NA
  
  opt <- nls.control(maxiter = 400, minFactor = 1e-10, tol = 1e-10)
  
  if (model_type == "model1") {
    
    mod <- minpack.lm::nlsLM(
      p_manip ~
        ps * (1 - exp(-alpha * light / ps)) * exp(-beta * light / ps) + p0,
      data = df,
      start = list(
        ps = 3,
        alpha = 0.01,
        beta = 0.01,
        p0 = 0.01
      ),
      lower = c(0, 1e-7, 1e-7, -Inf),
      control = opt
    )
    
  } else if (model_type == "model2") {
    
    mod <- minpack.lm::nlsLM(
      p_manip ~
        ps * (1 - exp(-alpha * light / ps)) + p0,
      data = df,
      start = list(
        ps = 3,
        alpha = 0.01,
        p0 = 0.01
      ),
      lower = c(0, 1e-7, -Inf),
      control = opt
    )
    
  }
  
  return(mod)
  
}

pp <- pp %>% 
  ungroup() %>% 
  group_by(date, depth, model_type) %>% 
  nest() %>% 
  mutate(mod = map2(data, model_type, fit_pe))

pred <- pp %>% 
  mutate(prediction = map2(data, mod, modelr::add_predictions)) %>% 
  unnest(prediction)

p <- pred %>% 
  ggplot(aes(x = light, y = p_manip)) +
  geom_point(aes(color = model_type)) +
  facet_wrap(~date + depth, scales = "free") +
  geom_line(aes(y = pred))

ggsave("graphs/pe_curves.pdf", width = 15, height = 10)

params <- pp %>% 
  mutate(params = map(mod, broom::tidy)) %>% 
  unnest(params) %>% 
  dplyr::select(date, depth, term, estimate, model_type) %>% 
  spread(term, estimate)

# Duplicate the first row and assume the same values at depth = 0 m

params <- params %>%
  group_by(date) %>%
  complete(depth = c(0, unique(depth))) %>%
  fill(model_type, alpha, beta, p0, ps, .direction = "up")

# Should be all equal
params %>% 
  filter(depth %in% c(0, 1))

# Propagate light ---------------------------------------------------------

df <- left_join(params, hourly_par, by = "date") %>%
  dplyr::select(-data) %>%
  rename(ed0 = e) %>% 
  left_join(kd_par, by = "date") %>% 
  mutate(edz = ed0 * exp(-kd_par * depth)) %>% 
  mutate(edz_4percent = edz * 0.04)

df %>% 
  ggplot(aes(x = hour, y = edz, group = interaction(date, depth))) +
  geom_line(aes(color = factor(depth))) +
  facet_wrap(~date, scales = "free_y") +
  geom_text(aes(x = -Inf, y = Inf, label = round(kd_par, digits = 2)), size = 2, vjust = 2, hjust = -2) +
  geom_line(aes(y = ed0))

ggsave("graphs/edz.pdf", width = 10)


# Calculate hourly PP -----------------------------------------------------

df <- df %>% 
  mutate(pp = ps * (1 - exp(-alpha * edz / ps))) %>% 
  mutate(pp_4percent = ps * (1 - exp(-alpha * edz_4percent / ps)))

p <- df %>% 
  ggplot(aes(x = hour, y = pp)) +
  geom_line() +
  geom_line(aes(y = pp_4percent, color = "Transmittance at 4%")) +
  facet_wrap(~date+depth, scales = "free") +
  theme(legend.position = "top") +
  theme(legend.title = element_blank()) +
  xlab("Time (hour)") +
  ylab("Primary prodction")

ggsave("graphs/hourly_pp.pdf", width = 15, height = 10)

# Calculate daily PP ------------------------------------------------------

depth_integrated_pp <- df %>% 
  group_by(date, depth) %>% 
  nest() %>% 
  mutate(pp = map(data, ~sum(.$pp))) %>% 
  mutate(pp_4percent = map(data, ~sum(.$pp_4percent))) %>% 
  unnest(pp, pp_4percent)

p <- depth_integrated_pp %>% 
  ggplot(aes(x = pp, y = depth)) +
  geom_point() +
  geom_path() +
  geom_path(aes(x = pp_4percent, color = "Transmittance at 4%")) +
  scale_y_reverse() +
  facet_wrap(~date, scales = "free") +
  theme(legend.position = "top") +
  theme(legend.title = element_blank()) +
  xlab("Primary production") +
  ylab("Depth (m)")
 
ggsave("graphs/depth_integrated_pp.pdf",  width = 12, height = 9)

# Calculate daily PP ------------------------------------------------------

daily_integrated_pp <- depth_integrated_pp %>% 
  group_by(date) %>% 
  nest() %>% 
  mutate(daily_pp = map(data, ~pracma::trapz(.$depth, .$pp))) %>% 
  mutate(daily_pp_4percent = map(data, ~pracma::trapz(.$depth, .$pp_4percent))) %>% 
  unnest(daily_pp, daily_pp_4percent)

daily_integrated_pp

# Save the data -----------------------------------------------------------

daily_integrated_pp %>% 
  dplyr::select(-data) %>% 
  write_csv("data/calculated_daily_pp.csv")


# Include all fonts -------------------------------------------------------

files <- list.files("graphs/", full.names = TRUE)
lapply(files, embed_fonts)

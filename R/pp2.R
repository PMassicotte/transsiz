# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  This is the script to process the second batch of samples. It 
#               is based on pp.R, but for more than 1 station.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

source("R/calculate_kd.R")

kd_par <- read_csv("data/kd_par.csv")

# *************************************************************************
# Read pyranometer data
# *************************************************************************

pyrano <- data.table::fread("data/PS92_cont_surf_Pyrano.txt") %>% 
  setNames(iconv(names(.), "latin1", "utf-8", sub = "byte")) %>% 
  janitor::clean_names() %>% 
  as_tibble() %>% 
  mutate(date_time = anytime::anytime(date_time)) %>% 
  mutate(date = lubridate::date(date_time)) %>% 
  mutate(date_numeric = lubridate::hour(date_time) * 3600 + lubridate::minute(date_time) * 60 + lubridate::second(date_time)) %>% 
  mutate(hour = lubridate::hour(date_time)) %>% 
  mutate(minute = lubridate::minute(date_time)) %>% 
  filter(par_just_below_surface_µmol >= 0)

pyrano <- pyrano %>% 
  group_by(date) %>% 
  nest() %>% 
  mutate(mod = map(data, ~smooth.spline(.$date_numeric, .$par_just_below_surface_µmol, spar = 0.5))) %>% 
  mutate(predicted = map(mod,  ~data_frame(x = .$x, y = .$y))) %>% 
  unnest(data, predicted)

p <- pyrano %>% 
  ggplot(aes(x = date_numeric, y = par_just_below_surface_µmol)) +
  geom_line() +
  facet_wrap(~date, scales = "free") +
  geom_line(aes(x = x, y = y), color = "red")

ggsave("graphs/pyrano.pdf", width = 15, height = 8)

res <- pyrano %>% 
  group_by(date, hour) %>% 
  nest() %>% 
  mutate(e = map(data, ~mean(.$par_just_below_surface_µmol))) %>% 
  unnest(e)

res %>% 
  ggplot(aes(x = hour, y = e)) +
  geom_line() +
  facet_wrap(~date, scales = "free")


# Primary production data -------------------------------------------------

pp <- readxl::read_excel("data/Data_PP_AllStations.xlsx", sheet = "water") %>% 
  fill(date, depth) %>% 
  janitor::clean_names() %>% 
  drop_na() %>% 
  rename(light = light_incub) %>% 
  mutate(light = if_else(light < 0, 0, light)) %>% 
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
        ps * tanh((alpha * light) / ps),
      data = df,
      start = list(
        ps = 0.5,
        alpha = 0.005
      ),
      lower = c(0, 0)
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

pred %>% 
  ggplot(aes(x = light, y = p_manip)) +
  geom_point(aes(color = model_type)) +
  facet_wrap(~date + depth, scales = "free") +
  geom_line(aes(y = pred))

params <- pp %>% 
  mutate(params = map(mod, broom::tidy)) %>% 
  unnest(params) %>% 
  select(date, depth, term, estimate, model_type) %>% 
  spread(term, estimate)


# Propagate light ---------------------------------------------------------

df <- left_join(params, res, by = "date") %>%
  select(-data) %>%
  rename(ed0 = e) %>% 
  left_join(kd_par, by = "date") %>% 
  mutate(edz = ed0 * exp(-kd_par * depth))

df %>% 
  ggplot(aes(x = hour, y = edz, group = interaction(date, depth))) +
  geom_line(aes(color = factor(depth))) +
  facet_wrap(~date, scales = "free_y") +
  geom_text(aes(x = -Inf, y = Inf, label = round(kd_par, digits = 2)), size = 2, vjust = 2, hjust = -2) +
  geom_line(aes(y = ed0))

ggsave("graphs/edz.pdf", width = 10)

rm(list = ls())

kd <- read_feather("data/clean/kd.feather")
light <- read_feather("data/clean/hourly_par.feather")

params <- read_feather("data/clean/photosynthetic_parameters.feather") %>% 
  select(date, depth, sheet, ps, alpha, p0, pm) %>% 
  mutate(date = as.Date(date))

## "Official" stations
stations <- readxl::read_excel("data/raw/Sampling_Takuvik.xlsx", skip = 1) %>% 
  janitor::clean_names() %>% 
  mutate(date = as.Date(date)) %>% 
  filter(p_vs_e == "x") %>% 
  select(station, date) %>% 
  mutate(station = gsub("/", "_", station)) %>% 
  separate(station, into = c("cruise", "station", "operation")) %>% 
  select(-cruise, -operation)

df <- stations %>% 
  left_join(kd, by = "station") %>% 
  left_join(light, by = "date") %>% 
  left_join(params, by = "date") %>% 
  drop_na()

# Water -------------------------------------------------------------------

water <- df %>% 
  filter(sheet == "water") %>% 
  mutate(depth = parse_number(depth)) %>% 
  group_by(station, hour) %>% 
  complete(depth = c(0, unique(depth))) %>%
  fill(date, kd, hour, e, sheet, ps, alpha, p0, .direction = "up") %>% 
  mutate(ez = e * exp(-kd * depth)) %>% 
  mutate(pp = ps * (1 - exp(-alpha * ez / ps)))
  
water %>% 
  ggplot(aes(x = ez, y = depth)) +
  geom_line(aes(color = factor(hour))) +
  facet_wrap(~station, scales = "free") +
  scale_y_reverse() 

water %>% 
  ggplot(aes(x = hour, y = pp, group = station)) +
  geom_point() +
  facet_grid(station~depth, scales = "free") 
  

daily_pp <- water %>% 
  group_by(station, depth) %>% 
  nest() %>% 
  mutate(pp = map(data, ~sum(.$pp))) %>% 
  unnest(pp)

daily_pp %>% 
  ggplot(aes(x = pp, y = depth)) +
  geom_point() +
  geom_path() +
  facet_wrap(~station, scales = "free") +
  scale_y_reverse() 


daily_pp %>% 
  group_by(station) %>% 
  nest() %>% 
  mutate(pp = map(data, ~pracma::trapz(.$depth, .$pp))) %>% 
  unnest(pp)


# ice ---------------------------------------------------------------------

## Daily PP is expressed per m³ because ice data can not be integrated over 
## depth. We are also using par just bellow surface (e) and we do not propagate 
## light in the water column like we did for water samples.

ice <- df %>% 
  filter(sheet == "ice") %>% 
  mutate(pp = ps * (1 - exp(-alpha * e / ps))) %>% 
  group_by(station, depth) %>% 
  nest() %>% 
  mutate(pp = map(data, ~sum(.$pp))) %>% 
  unnest(pp)

# uisw --------------------------------------------------------------------

uisw <- df %>% 
  filter(sheet == "UISW") %>% 
  mutate(depth = ifelse(grepl("UISW", depth), 0, depth)) %>% 
  mutate(depth = parse_number(depth)) %>% 
  mutate(ez = e * exp(-kd * depth)) %>% 
  mutate(pp = ps * (1 - exp(-alpha * ez / ps))) %>% 
  group_by(station, depth) %>% 
  nest() %>% 
  mutate(pp = map(data, ~sum(.$pp))) %>% 
  unnest(pp)

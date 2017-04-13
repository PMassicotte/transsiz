rm(list = ls())

source("R/zzz.R")

## Read limits used to extract the data linked to light profils (estimated by Flavienne)

limits <- read_csv("data/raw/bornes_profils_ed.csv") %>% 
  mutate(station = gsub("/", "_", station)) %>% 
  drop_na()

## ROV irradiance

files <- list.files("data/raw/Katlein-etal_2016/datasets/", "rov_irrad", full.names = TRUE)

kd <- lapply(files, read_irradiance) %>% 
  set_names(basename(files)) %>% 
  bind_rows(.id = "station") %>% 
  mutate(station = substr(station, 1, 10)) %>% 
  filter(wavelength >= 400 & wavelength <= 700) %>% 
  # mutate(np = irradiance * wavelength * 5.03e15) %>% 
  # mutate(eqf = np / 6022e23) %>% 
  mutate(e = irradiance * wavelength * 0.836e-2) %>% # https://www.berthold.com/en/bio/how-do-i-convert-irradiance-photon-flux
  group_by(station, date_time, dist_rel_x_m, dist_rel_y_m, depth_water_m) %>%
  summarise(par = sum(e))

p <- kd %>% 
  ggplot(aes(x = date_time, y = depth_water_m)) +
  geom_point() +
  facet_wrap(~station, scales = "free") +
  xlab("Depth (m)")

ggsave("graphs/rov_vs_depth.pdf", width = 12)

## Cut the data

kd <- kd %>% 
  inner_join(limits, by = "station") %>% 
  group_by(station) %>% 
  filter(date_time >= date_start & date_time <= date_end)

# kd %>% 
#   ggplot(aes(x = ed_w_m_2, y = depth_water_m)) +
#   geom_point(size = 1) +
#   facet_wrap(~station, scales = "free") +
#   scale_y_reverse()

res <- kd %>% 
  nest() %>% 
  mutate(model = map(data, ~nls(.$par ~ a0 * exp(-kd * .$depth_water_m), data = ., start = list(a0 = 5, kd = 0.15)))) %>% 
  mutate(pred = map2(data, model, modelr::add_predictions)) %>% 
  mutate(r2 = map2(data, pred, function(data, pred) {
    
    r2 <- cor(data$par, pred$pred)^2
    
  }))

p <- res %>% 
  unnest(pred) %>% 
  ggplot(aes(y = par, x = depth_water_m)) +
  geom_point(size = 1) +
  geom_line(aes(y = pred), color = "red") +
  facet_wrap(~station, scales = "free") +
  scale_y_reverse() +
  ylab(bquote("PAR ("~mu*mol%*%s^{-1}%*%m^{-2}~")")) +
  xlab("Depth (m)")


kd <- res %>% 
  mutate(params = map(model, broom::tidy)) %>% 
  unnest(params) %>% 
  filter(term == "kd")

p <- p + 
  geom_text(
    data = kd,
    aes(
      x = Inf,
      y = Inf,
      label = sprintf("Kd: %2.2f", estimate)
    ),
    vjust = -1,
    hjust = 1.1
  ) +
  geom_text(
    data = unnest(res, r2),
    aes(
      x = Inf,
      y = Inf,
      label = sprintf("R2: %2.4f", r2)
    ),
    vjust = -3,
    hjust = 1.1
  ) 

ggsave("graphs/kd.pdf")

kd %>% 
  separate(station, into = c("cruise", "station", "operation")) %>% 
  select(-cruise, -operation, -(std.error:p.value)) %>% 
  spread(term, estimate) %>% 
  write_feather("data/clean/kd.feather")

rm(list = ls())

source("R/zzz.R")

## Read limits used to extract the data linked to light profils (estimated by Flavienne)

limits <- read_csv2("data/raw/bornes_profils_ed.csv") %>% 
  mutate(station = gsub("/", "_", station))

## ROV irradiance

files <- list.files("data/raw/Katlein-etal_2016/datasets/", "rov_irrad", full.names = TRUE)

kd <- lapply(files, read_irradiance) %>% 
  set_names(basename(files)) %>% 
  bind_rows(.id = "station") %>% 
  mutate(station = substr(station, 1, 10)) %>% 
  filter(wavelength >= 400 & wavelength <= 700) %>% 
  group_by(station, date_time, dist_rel_x_m, dist_rel_y_m, depth_water_m) %>% 
  summarise(par = sum(irradiance))

p <- kd %>% 
  ggplot(aes(x = date_time, y = depth_water_m)) +
  geom_point() +
  facet_wrap(~station, scales = "free")

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
  mutate(pred = map2(data, model, modelr::add_predictions))

p <- res %>% 
  unnest(pred) %>% 
  ggplot(aes(y = par, x = depth_water_m)) +
  geom_point(size = 1) +
  geom_line(aes(y = pred), color = "red") +
  facet_wrap(~station, scales = "free") +
  scale_y_reverse() +
  ylab("PAR (W/m**2/nm)") +
  labs(title = paste0(
    strwrap(
      "Irradiance data were not converted to umol m-2 sec-1 since only used to calculate Kd",
      80
    ),
    sep = "",
    collapse = "\n"
  ))


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
  )

ggsave("graphs/kd.pdf")


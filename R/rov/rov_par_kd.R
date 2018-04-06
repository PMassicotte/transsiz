rm(list = ls())

source("R/zzz.R")

## Read limits used to extract the data linked to light profils (estimated by Flavienne)

limits <- read_csv("data/raw/bornes_profils_ed.csv") %>% 
  mutate(station = gsub("/", "_", station)) %>% 
  drop_na() %>% 
  mutate(station = stringr::str_extract(station, "(\\d{3}-\\d+)"))

## ROV irradiance
kd <- read_feather("data/clean/rov_irradiance.feather") %>% 
  filter(wavelength >= 400 & wavelength <= 700) %>% 
  # mutate(np = irradiance * wavelength * 5.03e15) %>% 
  # mutate(eqf = np / 6022e23) %>% 
  mutate(e = irradiance_w_m2_nm * wavelength * 0.836e-2) %>% # https://www.berthold.com/en/bio/how-do-i-convert-irradiance-photon-flux
  group_by(station, date_time, dist_rel_x_m, dist_rel_y_m, dist_sea_ice_bottom_m) %>%
  summarise(par = sum(e))

p <- kd %>% 
  ggplot(aes(x = date_time, y = dist_sea_ice_bottom_m)) +
  geom_point(size = 0.25) +
  facet_wrap(~station, scales = "free") +
  xlab("Depth (m)") +
  geom_vline(data = limits, aes(xintercept = date_start), color = "red") +
  geom_vline(data = limits, aes(xintercept = date_end), color = "red") +
  labs(title = "Depth of the ROV as function of time",
       subtitle = "The red lines represent the section (data) used to calculate kd value (see graphic named rov_kd.pdf).") +
  scale_y_reverse()

ggsave("graphs/rov_vs_depth.pdf", width = 12, height = 8)
ggsave("graphs/png/rov_vs_depth.png", width = 12, height = 8)

## Cut the data

kd <- kd %>% 
  inner_join(limits, by = "station") %>% 
  group_by(station) %>% 
  filter(date_time >= date_start & date_time <= date_end)

write_csv(kd, "data/clean/rov_par.csv")

res <- kd %>% 
  nest() %>% 
  mutate(model = map(data, ~nls(par ~ a0 * exp(-kd * dist_sea_ice_bottom_m), data = ., start = list(a0 = 5, kd = 0.15)))) %>% 
  mutate(pred = map2(data, model, modelr::add_predictions)) %>% 
  mutate(r2 = map2(data, pred, function(data, pred) {
    
    r2 <- cor(data$par, pred$pred)^2
    
  }))

p <- res %>% 
  unnest(pred) %>% 
  ggplot(aes(x = par, y = dist_sea_ice_bottom_m)) +
  geom_point(size = 1) +
  geom_line(aes(x = pred), color = "red") +
  facet_wrap(~station, scales = "free") +
  scale_y_reverse() +
  xlab(bquote("PAR ("~mu*mol%*%s^{-1}%*%m^{-2}~")")) +
  ylab("Depth (m)")

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
  ) +
  labs(
    title = "PAR values as a function of depth (rov data)",
    subtitle = "e = irradiance_w_m2_nm * wavelength * 0.836e-2",
    caption = "https://www.berthold.com/en/bio/how-do-i-convert-irradiance-photon-flux"
  )

ggsave("graphs/rov_kd.pdf")
ggsave("graphs/png/rov_kd.png")

## Save the data

kd %>% 
  write_csv("data/clean/rov_kd.csv")

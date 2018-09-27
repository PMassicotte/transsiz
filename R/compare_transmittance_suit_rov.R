rov <- read_feather("data/clean/rov_transmittance.feather") %>% 
  select(depth_water_m, transmittance, station) %>% 
  filter(depth_water_m <= 5) %>% 
  mutate(source = "rov") %>% 
  mutate(depth_water_m = -depth_water_m)

suit <- read_feather("data/clean/suit_transmittance.feather") %>% 
  select(depth_water_m = draft_m, transmittance, station) %>% 
  mutate(source = "suit") %>% 
  filter(depth_water_m >= -5)

df <- bind_rows(rov, suit)

df %>% 
  ggplot(aes(x = source, y = transmittance)) +
  geom_boxplot() +
  facet_wrap(~station) +
  scale_y_log10()

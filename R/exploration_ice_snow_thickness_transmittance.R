rm(list = ls())

thickness <- read_csv("data/clean/rov_ice_snow_thickness.csv")

thickness_mean <- thickness %>% 
  group_by(station) %>%
  mutate(optical_thickness = (1.5 * z_ice) + (10 * z_snow)) %>% 
  summarise(mean_optical_thickness = mean(optical_thickness, na.rm = TRUE))

rov <- read_feather("data/clean/rov_transmittance.feather")

rov %>% 
  ggplot(aes(y = transmittance, x = ice_thickness)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

rov_mean <- rov %>% 
  group_by(station) %>% 
  summarise(mean_transmittance = mean(transmittance)) %>% 
  mutate(device = "rov")

# suit_mean <- read_feather("data/clean/suit_transmittance.feather") %>%  
#   group_by(station) %>% 
#   summarise(mean_transmittance = mean(transmittance)) %>% 
#   mutate(device = "suit")

# trans <- bind_rows(rov_mean, suit_mean)

df <- inner_join(thickness_mean, rov_mean)

df %>% 
  # gather(type, mean_thickness, starts_with("z")) %>% 
  ggplot(aes(x = mean_optical_thickness, y = mean_transmittance)) +
  geom_point() +
  geom_smooth(method = "lm")

thickness %>% 
  ggplot(aes(x = reorder(factor(station), z_total), y = z_total)) +
  geom_boxplot()

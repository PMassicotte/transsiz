# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Density plot of ice and snow thickness
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv("data/clean/rov_ice_snow_thickness.csv") %>% 
  filter(station %in% c(19, 27, 31, 39, 43, 46, 47))

df %>%
  select(-z_total) %>%
  gather(layer, thickness, starts_with("z")) %>%
  filter(thickness >= 0) %>% 
  ggplot(aes(x = thickness, fill = layer)) +
  geom_density(aes(color = layer), alpha = 0.5, size = 0.25) +
  facet_wrap(~station) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1")

df %>%
  drop_na(z_ice) %>%
  ggplot(aes(x = z_ice)) +
  geom_density(fill = "#595959", color = "#595959") +
  facet_wrap( ~ station, scales = "free")


df <- read_csv("data/clean/suit_ice_snow_thickness.csv") %>% 
  filter(station %in% c(19, 27, 31, 32, 39, 43, 46, 47))

df %>% 
  ggplot(aes(x = z_ice)) +
  geom_density(fill = "#595959", color = "#595959") +
  facet_wrap(~station, scales = "free")
  
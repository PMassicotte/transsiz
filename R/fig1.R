rm(list = ls())

rov_transmittance <- read_feather("data/clean/rov_transmittance.feather") %>% 
  select(transmittance, station, depth_water_m) %>% 
  mutate(source = "rov")

suit_transmittance <- read_feather("data/clean/suit_transmittance.feather") %>% 
  select(transmittance, station, depth_water_m = draft_m) %>% 
  mutate(source = "suit")

df <- bind_rows(rov_transmittance, suit_transmittance) %>% 
  filter(station %in% c(19, 27, 31, 39, 43, 46, 47)) %>% 
  filter(depth_water_m <= 3)

p <- df %>% 
  mutate(source = str_to_title(source)) %>% 
  ggplot(aes(x = transmittance, fill = source, color = source)) +
  geom_density(alpha = 0.5) +
  scale_x_log10(labels = function(x) x * 100) +
  annotation_logticks(sides = "b", size = 0.25) +
  scale_y_continuous() +
  facet_wrap(~station, scales = "free_y") +
  xlab("Transmittance (%)") +
  ylab("Density") +
  labs(fill = "Device") +
  labs(color = "Device")
  # theme(legend.justification = c(0, 0)) +
  # theme(legend.position = c(0.4, 0.05))

ggsave("graphs/fig1.pdf", device = cairo_pdf, width = 7, height = 6.22 * 0.75)


# Boxplot? ----------------------------------------------------------------

df %>% 
  mutate(source = str_to_title(source)) %>% 
  ggplot(aes(x = source, y = transmittance)) +
  geom_boxplot(size = 0.25, outlier.size = 0.5) +
  facet_wrap(~station) +
  scale_y_log10(labels = function(x) x * 100) +
  annotation_logticks(sides = "l", size = 0.25) +
  ylab("Transmittance (%)") +
  theme(axis.title.x = element_blank())

ggsave("graphs/fig1b.pdf", device = cairo_pdf, width = 7, height = 6.22 * 0.75)

mod <- df %>% 
  filter(transmittance > 0) %>% 
  group_by(station) %>% 
  nest() %>% 
  mutate(mod = map(data, ~aov(log(.$transmittance) ~ .$source))) %>% 
  mutate(res = map(mod, summary)) 


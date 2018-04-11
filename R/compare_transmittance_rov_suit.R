rov <- read_feather("data/clean/rov_transmittance.feather")
suit <- read_feather("data/clean/suit_transmittance.feather")

rov <- rov %>% 
  select(station, cast, transmittance) %>% 
  mutate(source = "rov")

suit <- suit %>% 
  select(station, cast, transmittance) %>% 
  mutate(source = "suit")

df <- bind_rows(rov, suit)

df

p <- df %>% 
  ggplot(aes(x = transmittance, fill = source)) +
  geom_density(alpha = 0.5, size = 0.1) +
  facet_wrap(~station, scales = "free") +
  scale_x_log10(label = scales::percent) +
  annotation_logticks(sides = "b")

ggsave("graphs/suit_rov_transmittance.pdf", device = cairo_pdf, width = 12, height = 9)

df %>% 
  group_by(station, source) %>% 
  summarise(mean_transmittance = mean(transmittance, na.rm = TRUE)) %>% 
  spread(source, mean_transmittance)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Compare transmittance values between ROV and SUIT.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

rov_transmittance <- read_feather("data/clean/rov_transmittance.feather") %>% 
  select(transmittance, station) %>% 
  mutate(source = "rov")

suit_transmittance <- read_feather("data/clean/suit_transmittance.feather") %>% 
  select(transmittance, station) %>% 
  mutate(source = "suit")

df <- bind_rows(rov_transmittance, suit_transmittance) %>% 
  filter(station %in% c(19, 27, 31, 39, 43, 46, 47))

p1 <- df %>% 
  ggplot(aes(x = transmittance, color = source)) +
  geom_density() +
  scale_x_log10(labels = scales::comma) +
  annotation_logticks(sides = "b") +
  scale_y_continuous() +
  geom_vline(xintercept = c(0.1, 0.8), lty = 2)

p2 <- p1 +
  facet_wrap(~station, scales = "free_y")

p <- cowplot::plot_grid(p1, p2, ncol = 1, labels = "AUTO")

ggsave("graphs/transmittance_histograms_rov_vs_suit.pdf", device = cairo_pdf, height = 10)


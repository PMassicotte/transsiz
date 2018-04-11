# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Violin plot of PAR at 0 meter estimated from both ROV and SUIT transmittance.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

suit <- read_feather("data/clean/suit_propagated_par_water_column.feather") %>%
  mutate(data_source = "suit")

rov <- read_feather("data/clean/rov_propagated_par_water_column.feather") %>%
  mutate(data_source = "rov")

df <- bind_rows(suit, rov) %>%
  select(data_source, station, cast, hour, depth, par_z)

p <- df %>%
  filter(depth == 0) %>%
  filter(station != 32) %>%
  ggplot(aes(x = data_source, y = par_z, fill = data_source)) +
  geom_violin(size = 0.25) +
  facet_grid(station ~ hour) +
  scale_y_log10() +
  annotation_logticks(side = "l") +
  labs(title = "Boxplot comparing surface (0 m) hourly (0-23h) PAR  for all stations") +
  labs(subtitle = "The width of the viloin represent the distribution of the observations") +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 10))

ggsave("graphs/violin_plot_rov_suit_surface_par.pdf", device = cairo_pdf, width = 24, height = 8)

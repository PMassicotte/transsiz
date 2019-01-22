# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Plot of the ice/snow thickness
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

equal_breaks <- function(n = 3, s = 0.05, ...) {
  function(x) {
    # rescaling
    d <- s * diff(range(x)) / (1 + 2 * s)
    seq(min(x) + d, max(x) - d, length = n)
  }
}

df <- read_csv("data/clean/rov_ice_snow_thickness.csv")

# %>% 
#   filter(station %in% c(19, 27, 31, 39, 43, 46, 47))

p1 <- df %>%
  ggplot(aes(x = lon, y = lat, color = z_total)) +
  geom_point() +
  facet_wrap(~station, scales = "free") +
  scale_color_viridis_c(option = "D", limits = c(0.5, 2), oob = scales::squish) +
  scale_x_continuous(
    breaks = equal_breaks(n = 3, s = 0.05),
    labels = function(x)
      str_pad(round(x, digits = 3), width = 6, side = "right", pad = "0")
  ) +
  scale_y_continuous(
    breaks = equal_breaks(n = 3, s = 0.05),
    labels = function(x)
      str_pad(round(x, digits = 3), width = 6, side = "right", pad = "0")
  ) +
  labs(color = "Total thickness\n[snow + ice] (m)") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.spacing = unit(2, "lines")) +
  theme(legend.justification = c(0.5, 0.5)) +
  theme(legend.position = c(0.85, 0.1)) +
  theme(
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.direction = "horizontal"
  ) +
  guides(colour = guide_colourbar(title.position = "top", title.hjust = 0.5)) +
  theme(plot.margin = margin(r = 20))

p2 <- df %>% 
  gather(type, thickness, starts_with("z")) %>% 
  filter(thickness >= 0) %>% 
  ggplot(aes(x = factor(station), y = thickness, fill = type)) +
  geom_boxplot(outlier.size = 0.5, size = 0.25, position = position_dodge2(preserve = "single")) +
  xlab("Station") +
  ylab("Thickness (m)") +
  scale_fill_brewer(palette = "Set1", labels = c("Ice", "Snow", "Total")) +
  theme(legend.justification = c(1, 1), legend.position = c(0.95, 0.95)) +
  theme(legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(0, 10, by = 0.5))

p <- cowplot::plot_grid(p1, p2, ncol = 1, labels = "AUTO", rel_heights = c(1, 0.75))

ggsave("graphs/fig2.pdf", device = cairo_pdf, width = 7, height = 10)

# Stats -------------------------------------------------------------------

df %>%
  group_by(station) %>%
  summarise_at(
    .vars = vars(starts_with("z")),
    .funs = funs(
      mean(., na.rm = TRUE),
      min(., na.rm = TRUE),
      max(., na.rm = TRUE),
      sd(., na.rm = TRUE),
      n()
    )
  )

df %>%
  select(starts_with("z")) %>%
  skimr::skim()

## Problem: I only have the total thickness. How is it possible since Ilka said:
## Due to instrument failure of the Magna Probe no snow measurements were
## available for stations 46 and 47.

df %>%
  select(-z_total) %>%
  gather(layer, thickness, starts_with("z")) %>%
  filter(thickness >= 0) %>% 
  ggplot(aes(x = thickness, fill = layer)) +
  geom_density(aes(color = layer), alpha = 0.5) +
  facet_wrap(~station)

# Compare ROV and SUIT measures -------------------------------------------

ice_thickness_suit <- read_excel("data/raw/ice_snow_thickness/suit/TRANSSIZ_thickness.xlsx") %>%
  janitor::clean_names() %>%
  mutate(station = str_sub(stn, 1, 2)) %>%
  mutate(station = parse_number(station)) %>%
  filter(station %in% df$station)

df %>%
  drop_na(z_ice) %>%
  ggplot(aes(x = z_ice)) +
  geom_histogram() +
  facet_wrap(~station) +
  geom_vline(data = ice_thickness_suit, aes(xintercept = hi_m, color = "SUIT"), lty = 2) +
  theme(legend.title = element_blank())


# Compare ROV and SUIT ice thickness --------------------------------------

df1 <- read_csv("data/clean/rov_ice_snow_thickness.csv", guess_max = 1e6) %>% 
  mutate(device = "gem-2")

df2 <- read_csv("data/clean/suit_ice_snow_thickness.csv", guess_max = 1e6) %>% 
  mutate(device = "suit")

df <- bind_rows(df1, df2) %>% 
  filter(station %in% c(19, 27, 31, 39, 43, 46, 47))

df %>% 
  drop_na(z_ice) %>% 
  ggplot(aes(x = z_ice, fill = device, color = device)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~station, scales = "free") +
  xlab("Ice thickness (m)") +
  scale_x_continuous(breaks = seq(0, 10, by = 1))

df %>% 
  drop_na(z_ice) %>% 
  select(station, device, z_ice) %>% 
  group_by(station, device) %>%
  summarise(mean_z_ice = mean(z_ice, na.rm = TRUE)) %>% 
  spread(device, mean_z_ice)


df %>% 
  filter(station == 19 & device == "suit") %>% 
  drop_na(z_ice) %>% 
  distinct(z_ice)


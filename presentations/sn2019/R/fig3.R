rm(list = ls())

library(ggpmthemes)

theme_set(theme_poppins())

rov_transmittance <- read_feather("data/clean/rov_propagated_par_water_column.feather") %>% 
  filter(depth == 0 & hour == 0) %>% 
  select(station, depth, transmittance_ed0) %>% 
  mutate(source = "rov") 
  

suit_transmittance <- read_feather("data/clean/suit_propagated_par_water_column.feather") %>% 
  filter(depth == 0 & hour == 0) %>% 
  select(station, depth, transmittance_ed0) %>% 
  mutate(source = "suit")

df <- bind_rows(rov_transmittance, suit_transmittance) %>% 
  filter(station %in% c(19, 27, 31, 39, 43, 46, 47)) 

plain <- function(x, ...) {
  format(x * 100, ..., scientific = FALSE, drop0trailing = TRUE) %>%
    paste0("%")
}

numbers <- df %>%
  filter(station != 47) %>% 
  count(station, source) %>% 
  spread(source, n) %>% 
  mutate(n = glue::glue("ROV: {rov}\nSUIT: {suit}"))

p <- df %>% 
  filter(station != 47) %>% 
  mutate(source = toupper(source)) %>% 
  ggplot(aes(x = transmittance_ed0, fill = source)) +
  geom_density(alpha = 0.5, size = 0.25, color = NA) +
  scale_x_log10(labels = plain) +
  annotation_logticks(sides = "b", size = 0.25) +
  scale_y_continuous() +
  facet_wrap(~station, scales = "free_y") +
  xlab("Transmittance (%)") +
  ylab("Density") +
  labs(fill = "Device") +
  # labs(color = "Device") +
  geom_vline(xintercept = 0.1, lty = 2) +
  scale_fill_manual(values = c("#2B4257", "#5D99C6")) +
  # scale_color_brewer(palette = "Dark2") +
  geom_text(
    data = numbers,
    aes(
      x = 0,
      y = Inf,
      label = n,
      group = station
    ),
    inherit.aes = FALSE,
    parse = FALSE,
    hjust = -0.1,
    vjust = 1.5,
    size = 3.5
  ) +
  theme(panel.grid = element_blank()) +
  theme(plot.title = element_text(size = 14, lineheight = 1.5)) +
  theme(strip.background = element_blank()) +
  theme(axis.text=element_text(size = 12)) +
  theme(axis.title = element_text(size = 12)) +
  theme(strip.text = element_text(size = 12)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.position = "top")

ggsave("presentations/sn2019/graphs/fig3.pdf", device = cairo_pdf, height = 120, width = 190, units = "mm")

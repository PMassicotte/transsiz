rm(list = ls())

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
  count(station, source) %>% 
  spread(source, n) %>% 
  mutate(n = glue::glue("ROV: {rov}\nSUIT: {suit}"))

p <- df %>% 
  mutate(source = str_to_title(source)) %>% 
  ggplot(aes(x = transmittance_ed0, fill = source, color = source)) +
  geom_density(alpha = 0.5, size = 0.25) +
  scale_x_log10(labels = plain) +
  annotation_logticks(sides = "b", size = 0.25) +
  scale_y_continuous() +
  facet_wrap(~station, scales = "free_y") +
  xlab("Transmittance (%)") +
  ylab("Density") +
  labs(fill = "Device") +
  labs(color = "Device") +
  geom_vline(xintercept = 0.1, lty = 2) +
  scale_fill_brewer(palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
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
    size = 2
  )

ggsave("graphs/fig3.pdf", device = cairo_pdf, height = 190 / 1.61803398875, width = 190, units = "mm")

# Boxplot? ----------------------------------------------------------------

# df %>% 
#   mutate(source = str_to_title(source)) %>% 
#   ggplot(aes(x = source, y = transmittance_ed0)) +
#   geom_boxplot(size = 0.25, outlier.size = 0.5) +
#   facet_wrap(~station) +
#   scale_y_log10(labels = function(x) x * 100) +
#   annotation_logticks(sides = "l", size = 0.25) +
#   ylab("Transmittance (%)") +
#   theme(axis.title.x = element_blank())
# 
# ggsave("graphs/fig1b.pdf", device = cairo_pdf, width = 7, height = 6.22 * 0.75)
# 
# mod <- df %>% 
#   filter(transmittance_ed0 > 0) %>% 
#   group_by(station) %>% 
#   nest() %>% 
#   mutate(mod = map(data, ~aov(log(.$transmittance_ed0) ~ .$source))) %>% 
#   mutate(res = map(mod, summary)) 

# Stats for the paper -----------------------------------------------------

# df %>%
#   mutate(station = as.integer(station)) %>%
#   mutate(device = toupper(source)) %>% 
#   group_by(station, device) %>%
#   summarise(
#     min_transmittance = min(transmittance_ed0),
#     max_transmittance = max(transmittance_ed0),
#     mean_transmittance = mean(transmittance_ed0),
#     n_obs = n()
#   ) %>%
#   xtable::xtable(digits = 3, caption = "Descriptive statistics.") %>%
#   print(file = "article/jgr/tables/table3.tex", include.rownames = FALSE)

## Test with gt library

# df %>%
#   mutate(station = as.integer(station)) %>%
#   group_by(station, device = source) %>%
#   summarise(
#     min_transmittance = min(transmittance_ed0),
#     max_transmittance = max(transmittance_ed0),
#     mean_transmittance = mean(transmittance_ed0),
#     n_obs = n()
#   ) %>% 
#   gt::gt() %>% 
#   gt::tab_header(title = md("Data listing from **gtcars**")) %>% 
#   gt::as_latex() %>% 
#   as.character() 

df %>% 
  count(source)

df %>%
  mutate(station = as.integer(station)) %>%
  group_by(device = source) %>%
  summarise(
    min_transmittance = min(transmittance_ed0),
    max_transmittance = max(transmittance_ed0),
    mean_transmittance = mean(transmittance_ed0),
    n_obs = n()
  )

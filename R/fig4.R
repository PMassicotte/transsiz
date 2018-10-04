# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# What is the distribution of the errors of PP estimations?
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_feather("data/clean/primary_production_rov_vs_suit.feather")

df <- df %>% 
  group_by(station, data_source, cast, pp_source) %>% 
  summarise(mean_pp = mean(pp)) %>% 
  right_join(df) %>% 
  mutate(abs_err = abs(pp - mean_pp)) %>% 
  mutate(rel_err = abs(pp - mean_pp) / mean_pp)

plain <- function(x,...) {
  format(x * 100, ..., scientific = FALSE, drop0trailing = TRUE) %>% 
    paste0("%")
}

mean_rel_err <- df %>% 
  group_by(data_source, pp_source) %>% 
  summarise(mean_rel_err = mean(rel_err))

labels_pp_source <- c(
  daily_integrated_pp_mixing_model = "Mixing model",
  daily_integrated_pp_under_ice = "Under ice"
)

p2 <- df %>%
  ggplot(aes(x = rel_err)) +
  geom_histogram() +
  facet_grid(data_source ~ pp_source, scales = "free", labeller = labeller(data_source = str_to_upper, pp_source = labels_pp_source)) +
  scale_x_log10(labels = plain) +
  annotation_logticks(sides = "b") +
  geom_vline(
    data = mean_rel_err,
    aes(xintercept = mean_rel_err),
    lty = 2,
    color = "red",
    size = 0.25
  ) +
  geom_text(
    data = mean_rel_err,
    aes(
      x = mean_rel_err,
      y = Inf,
      label = paste0(round(mean_rel_err * 100, digits = 0), "%"),
      hjust = 1.2,
      vjust = 1.5
    ),
    size = 3
  ) +
  xlab("Relative error [abs(pp - mean_pp) / mean_pp] (%)")

ggsave("graphs/fig4.pdf", plot = p2, device = cairo_pdf, width = 7, height = 6.22 * 0.75)

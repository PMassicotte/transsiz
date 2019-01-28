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

round(range(df$abs_err), digits = 4)

mean_rel_err <- df %>% 
  group_by(data_source, pp_source) %>% 
  summarise(mean_rel_err = mean(rel_err)) %>% 
  mutate(pp_source = ifelse(str_detect(pp_source, "under_ice"), "italic(P)[underice]", "italic(P)[mixing]")) 

# labels_pp_source <- c(
#   daily_integrated_pp_mixing_model = "P[d]",
#   daily_integrated_pp_under_ice = "Underice"
# )

unique(df$pp_source)

p2 <- df %>%
  mutate(pp_source = ifelse(str_detect(pp_source, "under_ice"), "italic(P)[underice]", "italic(P)[mixing]")) %>% 
  ggplot(aes(x = rel_err)) +
  geom_histogram() +
  facet_grid(data_source ~ pp_source, scales = "free_y", labeller = labeller(data_source = str_to_upper, pp_source = label_parsed)) +
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
  xlab(bquote("Relative error"~(delta[italic(P)])))

ggsave("graphs/fig6.pdf", device = cairo_pdf, height = 190 / 1.61803398875, width = 190, units = "mm")
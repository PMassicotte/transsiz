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

p1 <- df %>% 
  ggplot(aes(x = abs_err)) +
  geom_histogram() +
  facet_grid(data_source ~ pp_source, scales = "free") +
  scale_x_log10() +
  annotation_logticks(sides = "b") +
  xlab(bquote("Absolute error [abs(mean_pp - pp)]" ~(mgC%*%m^{-2}%*%d^{-1})))

plain <- function(x,...) {
  format(x * 100, ..., scientific = FALSE, drop0trailing = TRUE) %>% 
    paste0("%")
}

p2 <- df %>% 
  ggplot(aes(x = rel_err)) +
  geom_histogram() +
  facet_grid(data_source ~ pp_source, scales = "free") +
  scale_x_log10(labels = plain) +
  annotation_logticks(sides = "b") +
  xlab("Relative error [abs(pp - mean_pp) / mean_pp]")

p <- cowplot::plot_grid(p1, p2, ncol = 1)
ggsave("graphs/fig2c.pdf", device = cairo_pdf, height = 12)

ggsave("graphs/fig4.pdf", plot = p2, device = cairo_pdf, width = 7, height = 6.22 * 0.75)

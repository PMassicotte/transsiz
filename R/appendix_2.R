# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Figures to show pyranometer data (appendix).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv("data/clean/pyranometer.csv")

p <- df %>% 
  ggplot(aes(x = hour, y = par_just_below_surface_µmol)) +
  geom_line() +
  geom_point() +
  facet_wrap(~station) +
  xlab("Time (hour)") +
  ylab(bquote(PAR~(mu*mol%*%m^{-2}%*%s^{-1}))) +
  scale_y_continuous(breaks = seq(0, 2000, by = 200))

ggsave("graphs/appendix_2.pdf", device = cairo_pdf, width = 8.7 * 0.75, height = 6.22 * 0.75)


# Stats -------------------------------------------------------------------

range(df$par_just_below_surface_µmol)

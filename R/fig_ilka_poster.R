# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Figure of PP for station 19 used in a poster (asked by Ilka).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

pp <- read_feather("data/clean/primary_production_rov_vs_suit.feather")

pp %>% 
  filter(station == 19) %>% 
  ggplot(aes(x = data_source, y = daily_integrated_pp_under_ice)) +
  geom_boxplot() +
  scale_y_log10() +
  annotation_logticks(sides = "l") +
  theme_bw(base_size = 20) +
  theme(axis.title.x = element_blank()) +
  ylab(bquote("Primary production"~(mgC~m^{-2})))

ggsave("~/Desktop/pp_station_19.png")

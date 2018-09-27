# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Figures to show propagated PAR in the water column (appendix).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_feather("data/clean/rov_propagated_par_water_column.feather") %>% 
  filter(station == 19) %>% 
  filter(depth <= 10) %>% 
  filter(hour %in% seq(0, 23, by = 4))

p <- df %>% 
  ggplot(aes(x = par_z_variable_transmittance, y = depth, group = id)) +
  geom_path(size = 0.1, alpha = 0.5) +
  scale_y_reverse() +
  facet_wrap(~hour) +
  ylab("Depth (m)") +
  xlab(bquote(PAR~(mu*mol%*%m^{-2}%*%s^{-1}))) 

ggsave("graphs/appendix_3.pdf", device = cairo_pdf, width = 8.7 * 0.75, height = 4)

## How many profils at station 19?
df %>% 
  count(station, hour, depth)

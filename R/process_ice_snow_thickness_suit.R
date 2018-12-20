rm(list = ls())

df <- read_table2("data/raw/ice_snow_thickness/suit/PS092_all_profiles_final.dat") %>% 
  janitor::clean_names() %>% 
  rename(station = stn) %>% 
  rename(z_ice = thick_m) %>% 
  mutate(station = str_sub(station, 1, 2))

df 

write_csv(df, "data/clean/suit_ice_snow_thickness.csv")

# df %>% 
#   ggplot(aes(x = thick_m)) +
#   geom_histogram(binwidth = 0.1) +
#   facet_wrap(~stn, scales = "free")

# unique(abs(df$draft_m - df$z_ice))

df %>%
  ggplot(aes(x = -draft_m, y = z_ice)) +
  geom_point() +
  geom_abline()

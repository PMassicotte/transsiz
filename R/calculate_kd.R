# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate KdPAR based on mean chla concentration. Equation
#               is from Morel 1998.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- readxl::read_excel("data/Profils_Chl_WaterStation.xlsx") %>%
  janitor::clean_names(case = "old_janitor") %>%
  mutate(chla_mg_m3 = sum_chl_ng_l / 1000)

# df %>%
#   ggplot(aes(x = chla_mg_m3, y = depth_water_m)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(~date, scales = "free") +
#   scale_y_reverse()

df <- df %>%
  group_by(station_sample, date) %>%
  summarise(mean_chla_mg_m3 = mean(chla_mg_m3)) %>%
  ungroup() %>%
  mutate(kd_par = 0.121 * mean_chla_mg_m3^0.428) %>%
  mutate(date = lubridate::date(date))

write_csv(df, "data/kd_par.csv")

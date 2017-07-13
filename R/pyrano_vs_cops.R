rm(list = ls())

cops <- read_feather("data/clean/cops.feather")
pyrano <- read_feather("data/clean/pyranometer.feather")

cops <- cops %>% 
  filter(wavelength >= 400 & wavelength <= 700) %>% 
  group_by(hour, date, date_time, file_name, depth) %>% 
  nest() %>% 
  mutate(par = map(data, ~pracma::trapz(.$wavelength, .$edz))) %>% 
  unnest(par)

cops %>% 
  ggplot(aes(x = depth, y = par / 1e-6)) +
  geom_line(aes(color = file_name)) +
  facet_wrap(~date_time, scales = "free") +
  scale_y_reverse()

cops <- cops %>% 
  filter(depth == 0)

p <- pyrano %>%
  ggplot(aes(x = date_time, y =  par_just_below_surface_µmol)) +
  geom_line(aes(color = "Pyranometer")) +
  facet_wrap( ~ date, scales = "free") +
  scale_x_datetime(
    date_labels = "%H:%M:%S",
    expand = c(0.2, 0),
    breaks = scales::pretty_breaks(n = 4)
  ) +
  geom_point(data = cops,
             aes(x = date_time, y = par / 1e-6, color = "C-OPS")) +
  ylab("PAR just above surface (umol s-1 m-2)") +
  xlab("Time (hour)") +
  labs(color = "Data source", 
       title = "Pyranometer vs C-OPS PAR",
       subtitle = "C-OPS PAR calculated at 0 meter. Multiple red dots mean there were many C-OPS measurements that day.") +
  scale_color_manual(values = c("Pyranometer" = "black", "C-OPS" = "red")) 

ggsave("graphs/pyrano_vs_cops_par.pdf", width = 18, height = 12)

cops %>% 
  filter(date == as.Date("2015-06-20"))

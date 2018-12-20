rm(list = ls())

options(future.globals.maxSize = 4294967296)

df <- read_feather("data/clean/primary_production_rov_vs_suit.feather")

res <- df %>% 
  group_by(station, data_source, pp_source) %>% 
  nest() %>% 
  # slice() %>% 
  mutate(average_pp = map_dbl(data, ~mean(.$pp))) %>% 
  mutate(random_pp = future_map(data, function(data) {
    
    f <- map(rep(1:250, each = 100), function(i) {
      
      sample_n(data, i, replace = TRUE) 
      
    })
    
    
  }, .progress = TRUE, .options = future_options(seed = .Random.seed[10]))) %>% 
  unnest(random_pp) %>% 
  mutate(n_observation = map_int(random_pp, nrow)) %>% 
  group_by(station, data_source, pp_source, average_pp, n_observation) %>% 
  mutate(simulation_number = 1:n())

res2 <- res %>% 
  group_by(data_source, pp_source, average_pp, n_observation, simulation_number) %>% 
  summarise(random_average_pp = future_map_dbl(random_pp, ~mean(.$pp))) %>% 
  mutate(abs_error = abs(average_pp - random_average_pp) / (abs(average_pp + random_average_pp) / 2)) %>% 
  group_by(data_source, pp_source, n_observation) %>% 
  summarise(mean_abs_error = mean(abs_error), sd_abs_error = sd(abs_error))


mod <- res2 %>%
  group_by(data_source, pp_source) %>%
  nest() %>%
  mutate(mod = map(
    data,
    ~ nls(
      .$mean_abs_error ~ a * exp(-k * .$n_random_sample) + offset,
      data = .,
      start = list(a = 100, k = 0.1, offset = 0)
    )
  )) %>%
  mutate(pred = map2(data, mod, modelr::add_predictions)) %>%
  unnest(pred)

res2 %>% 
  # filter(data_source == "suit" & pp_source == "daily_integrated_pp_under_ice") %>% 
  ggplot(aes(x = n_observation, y = mean_abs_error)) +
  # geom_line(data = res %>% filter(data_source == "suit" & pp_source == "daily_integrated_pp_under_ice"), aes(x = n_random_sample, y = abs_error, group = interaction(data_source, pp_source, simul_id, station)), alpha = 0.25) +
  # geom_ribbon(aes(ymin = mean_abs_error - sd_abs_error, ymax = mean_abs_error + sd_abs_error), alpha = 0.5, fill = "gray75") +
  geom_line(color = RColorBrewer::brewer.pal(3, "Set1")[2]) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 2, by = 0.1)) +
  facet_grid(data_source ~ pp_source)

+
  geom_line(aes(y = pred), color = RColorBrewer::brewer.pal(3, "Set1")[1])

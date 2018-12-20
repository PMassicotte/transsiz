rm(list = ls())

options(future.globals.maxSize = 4294967296)

df <- read_feather("data/clean/primary_production_rov_vs_suit.feather")

# set.seed(1234)
# 
# params <- list(n_obs = 1:250, n_replicate = )
# 
# df %>% 
#   group_by(station, data_source, pp_source) %>% 
#   nest() %>% 
#   mutate(res = map(data, function(data) {
#     
#     lapply(rep(1:20, each = 4), sample_n, tbl = data, replace = TRUE)
#     
#   })) %>% 
#   unnest(res)


# lapply(rep(1:20, each=4), sample_n, tbl=mtcars)


# res <- df %>% 
#   group_by(station, data_source, pp_source) %>% 
#   nest() %>% 
#   mutate(average_pp = map_dbl(data, ~mean(.$pp))) %>% 
#   mutate(random_pp = future_map(data, function(data) {
#     
#     lapply(rep(1:250, each = 10), sample_n, tbl = data, replace = TRUE) %>% 
#       bind_rows() %>% 
#       crossing(1:250, 1:10)
#      
#    }, .progress = TRUE, .options = future_options(seed = .Random.seed[10]))) 


res <- df %>% 
  group_by(station, data_source, pp_source) %>% 
  nest() %>% 
  mutate(average_pp = map_dbl(data, ~mean(.$pp))) %>% 
  crossing(n_random_sample = 1:250, simul_id = 1:100) %>% 
  mutate(random_average_pp = future_map2_dbl(data, n_random_sample, function(data, n) {

    data %>% 
      sample_n(n, replace = TRUE) %>% 
      summarise(mean_pp = mean(pp)) %>% 
      pull(mean_pp)
    
  }, .progress = TRUE, .options = future_options(seed = .Random.seed[10])))

res <- res %>% 
  mutate(abs_error = abs(average_pp - random_average_pp) / (abs(average_pp + random_average_pp) / 2)) 

res2 <- res %>% 
  mutate(abs_error = abs(average_pp - random_average_pp) / (abs(average_pp + random_average_pp) / 2)) %>% 
  group_by(data_source, pp_source, n_random_sample) %>% 
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

mod %>% 
  # filter(data_source == "suit" & pp_source == "daily_integrated_pp_under_ice") %>% 
  ggplot(aes(x = n_random_sample, y = mean_abs_error)) +
  # geom_line(data = res %>% filter(data_source == "suit" & pp_source == "daily_integrated_pp_under_ice"), aes(x = n_random_sample, y = abs_error, group = interaction(data_source, pp_source, simul_id, station)), alpha = 0.25) +
  geom_ribbon(aes(ymin = mean_abs_error - sd_abs_error, ymax = mean_abs_error + sd_abs_error), alpha = 0.5, fill = "gray75") +
  geom_point() +
  # geom_line(color = RColorBrewer::brewer.pal(3, "Set1")[2]) +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 2, by = 0.25)) +
  facet_grid(data_source ~ pp_source) +
  geom_line(aes(y = pred), color = RColorBrewer::brewer.pal(3, "Set1")[1])

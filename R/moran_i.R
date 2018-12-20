# http://www.petrkeil.com/?p=1050

library(pgirmess)

rm(list = ls())

rov <- read_feather("data/clean/rov_propagated_par_water_column.feather") %>% 
  filter(depth == 0 & hour == 12)

# %>% 
#   filter(station == 31 & hour == 12)

position <- read_feather("data/clean/rov_irradiance.feather") %>% 
  select(date_time, contains("_rel_")) %>% 
  distinct()

df <- inner_join(rov, position) %>% 
  group_by(station) %>% 
  sample_frac(1)

res <- df %>% 
  group_by(station) %>% 
  nest() %>% 
  mutate(moran = pbmclapply(data, function(x) {
    
    res <-
      pgirmess::correlog(
        coords = cbind(x$dist_rel_x_m, x$dist_rel_y_m),
        z = x$transmittance_ed0,
        method = "Moran",
        nbclass = 20
      ) %>% 
      as_tibble() %>% 
      janitor::clean_names()
    
    
  }, mc.cores = detectCores() - 1)) %>% 
  unnest(moran)


# Plot --------------------------------------------------------------------

p <- res %>% 
  ggplot(aes(x = dist_class, y = coef)) +
  geom_hline(yintercept = 0, lty = 2, color = "red", size = 0.25) +
  geom_line() +
  geom_point(aes(color = ifelse(p_value <= 0.05, TRUE, FALSE))) +
  facet_wrap(~station) +
  scale_x_continuous(breaks = seq(0, 400, by = 50)) +
  xlab("Distance lag (m)") +
  ylab("Moran's I") +
  scale_color_manual(values = c("TRUE" = "firebrick1", "FALSE" = "black")) +
  labs(color = "Significant values\nat p < 0.05") +
  theme(legend.justification = c(0.5, 0.5), legend.position = c(0.85, 0.15))

ggsave("graphs/fig7.pdf", device = cairo_pdf, width = 8.7 * 0.75, height = 5)


# Some stats --------------------------------------------------------------

res %>% 
  group_by(station) %>% 
  filter(dist_class == min(dist_class)) %>% 
  arrange(coef)

## What is the average patch size, defined as as the distance class at which the
## first zero value of Moran’s I occurred in the correlograms

patch <- res %>% 
  group_by(station) %>% 
  filter(coef <= 0) %>% 
  slice(1) %>% 
  ungroup()

patch %>% 
  summarise_at(.vars = "dist_class", .funs = c("min", "max", "mean"))
  
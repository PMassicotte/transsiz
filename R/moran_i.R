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

res %>% 
  ggplot(aes(x = dist_class, y = coef)) +
  geom_hline(yintercept = 0, lty = 2, color = "red", size = 0.25) +
  geom_line() +
  geom_point() +
  facet_wrap(~station) +
  scale_x_continuous(breaks = seq(0, 400, by = 50)) +
  xlab("Distance (m)") +
  ylab("Moran's I")

ggsave("graphs/appendix_5.pdf", device = cairo_pdf, width = 8.7 * 0.75, height = 5)

# 
# ncf.cor <-
#   correlog(
#     res$dist_rel_x_m,
#     res$dist_rel_y_m,
#     res$transmittance_ed0,
#     increment = 10,
#     resamp = 0
#   )
# 
# plot(ncf.cor)


# library(pgirmess)
# df <- ee$data[[5]]
# pgi.cor <- pgirmess::correlog(coords=cbind(df$dist_rel_x_m, df$dist_rel_y_m), z=df$transmittance_ed0, method="Moran", nbclass=21)
 

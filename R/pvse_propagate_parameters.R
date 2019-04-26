# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Propagate PvsE parameters between 0 and 15 meters.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

# PvsE curve data ---------------------------------------------------------

pvse <- read_csv("data/clean/photosynthetic_parameters.csv") %>%
  filter(sheet == "water") %>%
  select(date, depth, ps, alpha, beta) %>%
  mutate(depth = parse_number(depth)) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date, depth)


## Only date is provided in PvsE data, use another file to find the
## corresponding station.

station <- readxl::read_excel("data/raw/station_cast_date_pvse.xls") %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(station = gsub("PS92/", "", station)) %>% 
  separate(station, into = c("station", "cast"), convert = TRUE)

pvse <- pvse %>% 
  left_join(station, by = "date")

## Complete to add depth = 0 meter

pvse <- pvse %>%
  group_by(station, cast) %>%
  complete(depth = c(0, unique(depth))) %>%
  fill(date, ps, alpha, beta, .direction = "up")

## Interpolate PvsE parameters in the water column betwen 0-40m at 1m increment.

pvse <- pvse %>%
  group_by(station, cast) %>%
  nest() %>%
  mutate(pred = map(data, function(df) {
    depth <- seq(0, 40, by = 1)

    af <- approxfun(df$depth, df$ps)
    ps <- af(depth)

    af <- approxfun(df$depth, df$alpha)
    alpha <- af(depth)
    
    af <- approxfun(df$depth, df$beta)
    beta <- af(depth)

    return(tibble(depth, ps, alpha, beta))
  }))

# Plot --------------------------------------------------------------------

p1 <- ggplot() +
  geom_path(data = unnest(pvse, data), aes(x = ps, y = depth, color = "raw")) +
  geom_point(data = unnest(pvse, data), aes(x = ps, y = depth, color = "raw")) +
  geom_point(data = unnest(pvse, pred), aes(x = ps, y = depth, color = "depth interpolated")) +
  scale_y_reverse() +
  facet_wrap(~ station) +
  theme(legend.title = element_blank()) +
  labs(title = "PvsE ps parameter") +
  labs(subtitle = "Blue observations were calculated from PvsE curves at bottle depth.")

p2 <- ggplot() +
  geom_path(data = unnest(pvse, data), aes(x = alpha, y = depth, color = "raw")) +
  geom_point(data = unnest(pvse, data), aes(x = alpha, y = depth, color = "raw")) +
  geom_point(data = unnest(pvse, pred), aes(x = alpha, y = depth, color = "depth interpolated")) +
  scale_y_reverse() +
  facet_wrap(~ station) +
  theme(legend.title = element_blank()) +
  labs(title = "PvsE alpha parameter") +
  labs(subtitle = "Blue observations were calculated from PvsE curves at bottle depth.")

p <- cowplot::plot_grid(p1, p2, ncol = 1, align = "hv", labels = "AUTO")
ggsave("graphs/pvse_propagated_parameters.pdf", device = cairo_pdf, height = 10, width = 10)

# Export ------------------------------------------------------------------

pvse <- pvse %>%
  unnest(pred)

write_csv(pvse, "data/clean/pvse_propagated_parameters.csv")

# Stats for the paper -----------------------------------------------------

ek <- read_csv("data/clean/photosynthetic_parameters.csv") %>%
  filter(sheet == "water") 

range(ek$ek)
mean(ek$ek)

ek %>% 
  summarise_if(is.numeric, .funs = funs(n(), mean))

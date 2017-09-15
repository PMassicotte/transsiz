rm(list = ls())


# Pyrano ------------------------------------------------------------------

pyrano <- read_csv("data/clean/pyranometer.csv")

# P vs E curves -----------------------------------------------------------

pvse <- read_csv("data/clean/photosynthetic_parameters.csv") %>% 
  filter(sheet == "water") %>% 
  select(date, depth, ps, alpha) %>% 
  mutate(depth = parse_number(depth)) %>% 
  mutate(date = as.Date(date))

pvse %>% 
  distinct(date)

## Change PvsE curves date to match those of the ROV
pvse <- pvse %>% 
  mutate(date = if_else(date == as.Date("2015-05-31"), as.Date("2015-06-01"), date)) %>% 
  mutate(date = if_else(date == as.Date("2015-06-03"), as.Date("2015-06-04"), date)) %>%
  mutate(date = if_else(date == as.Date("2015-06-06"), as.Date("2015-06-07"), date)) %>%
  mutate(date = if_else(date == as.Date("2015-06-11"), as.Date("2015-06-12"), date)) %>% 
  mutate(date = if_else(date == as.Date("2015-06-17"), as.Date("2015-06-18"), date)) %>% 
  mutate(date = if_else(date == as.Date("2015-06-19"), as.Date("2015-06-20"), date))

pvse <- pvse %>% 
  filter(date %in% unique(pyrano$date))

# kd ----------------------------------------------------------------------

kd <- read_csv("data/clean/rov_kd.csv") %>% 
  select(station, term, term, estimate) %>% 
  spread(term, estimate)

# transmittance -----------------------------------------------------------

transmittance <- read_feather("data/clean/rov_transmittance.feather") %>% 
  inner_join(kd, by = "station") %>% 
  filter(dist_sea_ice_bottom_m <= 3) ## The model is not good at predicting at higher depth

## Let's calculate transmittance at 0 m (ed0)

transmittance <- transmittance %>% 
  mutate(transmittance_percent_ed0 = transmittance_percent / (exp(-kd * dist_sea_ice_bottom_m)))

df <- transmittance %>% 
  select(station, dist_sea_ice_bottom_m, depth_water_m, transmittance_percent, transmittance_percent_ed0) %>% 
  distinct() %>% 
  gather(type, transmittance, -dist_sea_ice_bottom_m, -depth_water_m, -station)

df %>% 
  group_by(station, type) %>% 
  summarise(n = n())

m <- df %>% 
  group_by(type, station) %>% 
  summarise(mean_t = mean(transmittance, na.rm = TRUE))

m %>% 
  spread(type, mean_t) %>% 
  mutate(percent_increase = ((transmittance_percent_ed0 - transmittance_percent) / transmittance_percent_ed0) * 100)

df %>% 
  ggplot(aes(x = transmittance, fill = type)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~station, scales = "free") +
  geom_vline(data = m, aes(xintercept = mean_t, color = type)) +
  ggrepel::geom_text_repel(data = m, aes(x = mean_t, y = Inf, color = type, label = round(mean_t, digits = 2)))

# Merge all the data ------------------------------------------------------

df <- transmittance %>% 
  inner_join(pyrano) %>% 
  inner_join(pvse)


# Calculate PP ------------------------------------------------------------

column_integrated_pp <- function(df) {
  
  ## Complete to add depth = 0
  # res <- tidyr::complete(df, depth = c(0, unique(depth)))
  # res <- fill(df, everything(), .direction = "up")

  # res <- mutate(res, edz = par_just_below_surface_µmol * exp(-kd * depth) * transmittance_percent_ed0) 
  # res <- mutate(res, pp = ps * (1 - exp(-alpha * edz / ps)))

  
  ## data.table version (faster)
    
  res <- setDT(df)[CJ(depth = c(0,unique(depth))), on = 'depth'
                ][, c(1:16) := lapply(.SD, zoo::na.locf, fromLast = TRUE), .SDcols = 1:16][]
  
  res[, edz := par_just_below_surface_µmol * exp(-kd * depth) * transmittance_percent_ed0]
  res[, pp := ps * (1 - exp(-alpha * edz / ps))]
  
  column_integrated_pp <- pracma::trapz(res$depth, res$pp)
  
  return(column_integrated_pp)
  
}

setDT(df)

## PP intagrated over the water column (~20 min)
df <- df[, column_integrated_pp(.SD), by = .(station, date_time, hour)]

# df <- df %>%
#   # filter(filename == "PS92_019-6_rovT_irrad.tab") %>%
#   group_by(station, date_time, hour) %>%
#   nest()
# 
# ## PP intagrated over the water column (~20 min)
# df <- df %>% 
#   mutate(column_integrated_pp = map(data, column_integrated_pp)) %>% 
#   unnest(column_integrated_pp)

## PP integrated over 24h (daily pp)
res <- df %>% 
  group_by(station, date_time) %>% 
  summarise(daily_pp = pracma::trapz(hour, V1), n = n())

res %>% 
  ggplot(aes(x = daily_pp)) +
  geom_histogram() +
  facet_wrap(~station, scales = "free") +
  xlab(bquote("Primary production"~(mgC~m^{-3}~d^{-1})))

ggsave("graphs/pp_rov.pdf")

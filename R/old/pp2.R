# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  This is the script to process the second batch of samples. It
#               is based on pp.R, but for more than 1 station.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())


# Kd from chla ------------------------------------------------------------

source("R/calculate_kd.R")

kd_par <- read_csv("data/kd_par.csv") %>%
  dplyr::select(-station_sample)

# Propagate light ---------------------------------------------------------

df <- left_join(params, hourly_par, by = "date") %>%
  dplyr::select(-data) %>%
  rename(ed0 = e) %>%
  left_join(kd_par, by = "date") %>%
  mutate(edz = ed0 * exp(-kd_par * depth)) %>%
  mutate(edz_ice = e_ice * exp(-kd_par * depth))

df %>%
  ggplot(aes(x = hour, y = edz, group = interaction(date, depth))) +
  geom_line(aes(color = factor(depth))) +
  facet_wrap(~ date, scales = "free_y") +
  geom_text(aes(x = -Inf, y = Inf, label = round(kd_par, digits = 2)), size = 2, vjust = 2, hjust = -2) +
  geom_line(aes(y = ed0))

ggsave("graphs/edz.pdf", width = 10)


# Calculate hourly PP -----------------------------------------------------

df <- df %>%
  mutate(pp = ps * (1 - exp(-alpha * edz / ps))) %>%
  mutate(pp_4percent = ps * (1 - exp(-alpha * edz_ice / ps)))

p <- df %>%
  ggplot(aes(x = hour, y = pp)) +
  geom_line() +
  geom_line(aes(y = pp_4percent, color = "Transmittance at 4%")) +
  facet_wrap(~ date + depth, scales = "free") +
  theme(legend.position = "top") +
  theme(legend.title = element_blank()) +
  xlab("Time (hour)") +
  ylab("Primary prodction")

ggsave("graphs/hourly_pp.pdf", width = 15, height = 10)

# Calculate daily PP ------------------------------------------------------

depth_integrated_pp <- df %>%
  group_by(date, depth) %>%
  nest() %>%
  mutate(pp = map(data, ~ sum(.$pp))) %>%
  mutate(pp_4percent = map(data, ~ sum(.$pp_4percent))) %>%
  unnest(pp, pp_4percent)

p <- depth_integrated_pp %>%
  ggplot(aes(x = pp, y = depth)) +
  geom_point() +
  geom_path() +
  geom_path(aes(x = pp_4percent, color = "Transmittance at 4%")) +
  scale_y_reverse() +
  facet_wrap(~ date, scales = "free") +
  theme(legend.position = "top") +
  theme(legend.title = element_blank()) +
  xlab("Primary production") +
  ylab("Depth (m)")

ggsave("graphs/depth_integrated_pp.pdf", width = 12, height = 9)

## Figure ofr Ilka
depth_integrated_pp %>%
  ggplot(aes(x = pp_4percent, y = depth, color = factor(date))) +
  geom_point() +
  geom_path() +
  scale_y_reverse() +
  labs(caption = "Using 4% transmittance") +
  theme(legend.position = "top") +
  theme(legend.title = element_blank()) +
  xlab(bquote("Primary production" ~ (mgC ~ m^{
    -3
  } ~ d^{
    -1
  }))) +
  ylab("Depth (m)")

ggsave("graphs/depth_integrated_pp2.pdf")
embed_fonts("graphs/depth_integrated_pp2.pdf")

depth_integrated_pp %>%
  select(date, depth, pp_4percent) %>%
  write_csv("/home/pmassicotte/Desktop/data.csv")

# Calculate daily PP ------------------------------------------------------

daily_integrated_pp <- depth_integrated_pp %>%
  group_by(date) %>%
  nest() %>%
  mutate(daily_pp = map(data, ~ pracma::trapz(.$depth, .$pp))) %>%
  mutate(daily_pp_4percent = map(data, ~ pracma::trapz(.$depth, .$pp_4percent))) %>%
  unnest(daily_pp, daily_pp_4percent)

daily_integrated_pp

# Save the data -----------------------------------------------------------

daily_integrated_pp %>%
  dplyr::select(-data) %>%
  write_csv("data/calculated_daily_pp.csv")

# Include all fonts -------------------------------------------------------

files <- list.files("graphs/", full.names = TRUE)
lapply(files, embed_fonts)

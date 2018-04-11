# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Process and clean up SUIT data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

read_irradiance <- function(file) {

  # file <- "data/raw/suit/Irra_Rad_profiles/Ir_Dist_draft01_b.dat"

  df <- read_table2(file) %>%
    janitor::clean_names() %>%
    mutate(haul = str_match(file, "draft(\\d+)")[2]) %>%
    mutate(haul = parse_number(haul))

  return(df)
}

read_radiance <- function(file) {
  # file <- "data/raw/suit/Irra_Rad_profiles/Rad_Dist_draft01.dat"

  df <- read.table(file) %>%
    janitor::clean_names() %>%
    mutate(haul = str_match(file, "draft(\\d+)")[2]) %>%
    mutate(haul = parse_number(haul)) %>%
    as_tibble()

  return(df)
}

irradiance_files <- list.files("data/raw/suit/Irra_Rad_profiles/", pattern = "^ir", ignore.case = TRUE, full.names = TRUE)
radiance_files <- list.files("data/raw/suit/Irra_Rad_profiles/", pattern = "^rad", ignore.case = TRUE, full.names = TRUE)

irradiance <- map(irradiance_files, read_irradiance) %>%
  bind_rows()

radiance <- map(radiance_files, read_radiance) %>%
  bind_rows()

df <- full_join(irradiance, radiance, by = c("time_sec", "dist_m", "draft_m", "haul"))

# Geographic positions ----------------------------------------------------

files <- list.files("data/raw/suit/Lat_lon/", full.names = TRUE)

read_latlon <- function(file) {

  # file <- files[1]

  read_table2(file) %>%
    janitor::clean_names() %>%
    mutate(haul = str_match(file, "\\d{2}")) %>%
    mutate_all(parse_number)
}

latlon <- map(files, read_latlon) %>%
  bind_rows()

# Associate haul number to station id -------------------------------------

station <- read_table2("data/raw/suit/Summary_file_PS92.txt") %>%
  select(station = stn, haul, date = date_time) %>%
  mutate(date = (lubridate::parse_date_time(date, order = "mdY", tz = "UTC"))) %>%
  mutate(haul = parse_number(haul)) %>%
  separate(station, into = c("station", "cast"), sep = 2, convert = TRUE)

station

# Merge -------------------------------------------------------------------

res <- inner_join(df, station) %>%
  inner_join(latlon)

res <- res %>%
  mutate(date_time = date + lubridate::seconds_to_period(time_sec)) %>%
  select(-contains("int_time"))

# Change station ----------------------------------------------------------

## After discussion with Ilka, it was decided to different SUIT stations to
## match the ROV data when there was not a direct match.

#   | ROV 	| SUIT    	|                                           	|
#   |-----	|---------	|-------------------------------------------	|
#   | 19  	| 19      	|                                           	|
#   | 27  	| 27      	|                                           	|
#   | 31  	| **28**  	| No match, so use a different SUIT station 	|
#   | 32  	|         	| We forget this station                    	|
#   | 39  	| 39      	|                                           	|
#   | 43  	| 43      	|                                           	|
#   | 46  	| ** 45** 	| No match, so use a different SUIT station 	|
#   | 47  	| 47      	|                                           	|

res <- res %>%
  mutate(station = ifelse(station == 28, 31, station)) %>%
  mutate(station = ifelse(station == 45, 46, station))

# Final clean up ----------------------------------------------------------

res <- res %>%
  filter(dplyr::between(transmittance, 0, 1))

# Save data ---------------------------------------------------------------

write_feather(res, "data/clean/suit_transmittance.feather")

# Plot --------------------------------------------------------------------

p1 <- res %>%
  ggplot(aes(x = transmittance)) +
  geom_histogram() +
  facet_wrap(~ station, scales = "free") +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Histograms of transmittance measured by the SUIT device") +
  labs(subtitle = sprintf("Total of %d measurements", nrow(res)))

p2 <- res %>%
  ggplot(aes(x = date_time, y = draft_m)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ station, scales = "free") +
  labs(title = "Depth over the time")

p <- cowplot::plot_grid(p1, p2, ncol = 1, align = "hv", labels = "AUTO")
ggsave("graphs/suit_transmittance_histogram.pdf", device = cairo_pdf, height = 12, width = 10)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Process and clean up SUIT data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

df <- read_csv("data/raw/suit/PS92_all_irrad.csv") %>%
  janitor::clean_names() %>%
  rename(station = stn) %>% 
  mutate(station = as.character(station)) %>%
  mutate(station = substr(station, 1, 2)) %>% 
  mutate(station = parse_number(station)) %>% 
  rename(dist_m = dist) %>% 
  rename(draft_m = draft)

df

# Cleanup bad data --------------------------------------------------------

## Based on email discussions with Giulia, we choose 15 degrees. This is th
df <- df %>%
  filter(between(abs(incl_x_deg), 0, 15) & between(abs(incl_y_deg), 0, 15))

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

# Merge -------------------------------------------------------------------

## Need a rolling join because the dist_m is not exactly the same

# https://stackoverflow.com/questions/33438082/join-two-dataframes-with-the-closest-date-and-exact-string
# You have to make sure that the Date is the last key in both tables 
setkey(setDT(df), haul, dist_m)
setkey(setDT(latlon), haul, dist_m)

res <- latlon[df, roll = "nearest"] %>% 
  as_tibble()

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

res <- res %>% 
  filter(station %in% c(19, 27, 31, 32, 39, 43, 46, 47))

# Final clean up ----------------------------------------------------------

res <- res %>%
  filter(dplyr::between(transmittance, 0, 1))

# Save data ---------------------------------------------------------------

write_feather(res, "data/clean/suit_transmittance.feather")

# Plot --------------------------------------------------------------------

p1 <- res %>%
  ggplot(aes(x = transmittance)) +
  geom_histogram() +
  facet_wrap(~ station, scales = "free_y") +
  scale_x_log10() +
  # scale_x_continuous(labels = scales::percent) +
  annotation_logticks(sides = "b") +
  labs(title = "Histograms of transmittance measured by the SUIT device") +
  labs(subtitle = sprintf("Total of %d measurements", nrow(res)))

p2 <- res %>%
  ggplot(aes(x = time_sec, y = draft_m)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ station, scales = "free") +
  labs(title = "Depth over the time")

p <- cowplot::plot_grid(p1, p2, ncol = 1, align = "hv", labels = "AUTO")
ggsave("graphs/suit_transmittance_histogram.pdf", device = cairo_pdf, height = 12, width = 10)

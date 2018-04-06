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

write_feather(res, "data/clean/suit_transmittance.feather")

# Plot --------------------------------------------------------------------

p <- res %>%
  ggplot(aes(x = transmittance)) +
  geom_histogram() +
  facet_wrap(~ station, scales = "free") +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Histograms of transmittance measured by the SUIT device") +
  labs(subtitle = sprintf("Total of %d measurements", nrow(res)))

ggsave("graphs/suit_transmittance_histogram.pdf", device = cairo_pdf)

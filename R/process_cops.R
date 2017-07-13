rm(list = ls())

read_cops <- function(file) {
  
  meta <- read_lines(file, n_max = 7)
  
  date <- stringr::str_match(meta, "/day=(.*)")[, 2] %>% 
    na.omit() %>% 
    as.Date()
  
  hour <- stringr::str_match(meta, "/hour=(.*)")[, 2] %>% 
    na.omit() %>% 
    parse_number()
  
  df <-
    read_delim(
      file,
      delim = ",",
      skip = 8,
      col_names = c("depth", paste0("wl_", 305:700)),
      col_types = cols(.default = col_double())
    ) %>%
    gather(wavelength, edz, -depth) %>%
    mutate(wavelength = parse_number(wavelength)) %>%
    add_column(date, .before = "depth") %>%
    add_column(hour, .after = "date") %>% 
    mutate(hour = paste0(hour, ":00:00")) %>% 
    mutate(date_time = lubridate::parse_date_time(paste(date, hour), "%Y-%m-%d %H:%M:%S"))
  
  return(df)
  
}

# All COPS files ----------------------------------------------------------

files <- list.files("/media/work/Dropbox/TRANSSIZ_2015/light_fields_all_deployments_v2/", full.names = TRUE, pattern = "Ein.txt")

df <- map(files, read_cops) %>% 
  set_names(basename(files)) %>% 
  bind_rows(.id = "file_name") %>% 
  select(-date, -hour)

p <- df %>% 
  filter(wavelength %in% round(seq(305, 700, length.out = 25))) %>%
  filter(depth <= 50) %>% 
  ggplot(aes(x = edz, y = depth, group = wavelength)) +
  geom_line(aes(color = factor(wavelength))) +
  scale_y_reverse() +
  facet_wrap(~file_name, scales = "free") +
  labs(color = "Wavelengths") +
  theme(strip.text.x = element_text(size = 6)) +
  xlab("Irradiance E/m^2/nm") +
  ylab("Depth (m)")

p

# COPS GPS ----------------------------------------------------------------

## Read all GPS data

files <- list.files("/media/work/Dropbox/TRANSSIZ_2015/GPS_COPS_CSV/", full.names = TRUE)

gps <- map(files, read_csv, col_names = c("latitude", "longitude", "date", "hour")) %>% 
  bind_rows() %>% 
  mutate(date_time = lubridate::ymd_hms(paste(date, hour)))

gps %>% 
  distinct(longitude, latitude) %>% 
  ggplot(aes(x = longitude, y = latitude)) +
  geom_polygon(data = rworldmap::getMap(), aes(x = long, y = lat, group = group)) +
  geom_point(color = "red") 
  
## Match gps and cops using the closest date
setDT(df)
setDT(gps)
setkey(df, date_time)
setkey(gps, date_time)

df <- gps[df, roll = "nearest"] %>% 
  as.tibble()

df %>%
  distinct(longitude, latitude, .keep_all = TRUE) %>% 
  ggplot(aes(x = longitude, y = latitude)) +
  geom_polygon(data = rworldmap::getMap(resolution = "high"), aes(x = long, y = lat, group = group)) +
  geom_point(aes(color = factor(date))) +
  coord_fixed(xlim = c(5, 20), ylim = c(75, 85)) +
  labs(color = "Date")

ggsave("graphs/cops_gps.pdf")  

## Check the time distance

# td <- df %>% 
#   mutate(date_time_gps = lubridate::ymd_hms(paste(date, hour))) %>% 
#   mutate(date_diff = date_time - date_time_gps)
# 
# td %>% 
#   ggplot(aes(x = date_diff)) +
#   geom_histogram(binwidth = 1) +
#   xlab("Time difference in minutes")
# 
# ggsave("graphs/cops_gps_time_difference.pdf")

## Associate COPS data with station name

stations <- read_feather("data/clean/stations.feather") %>% 
  select(date, station) %>% 
  distinct()

df <- df %>% 
  left_join(stations, by = "date")

## only keep cops of interest (ie. at station where we have other data)

df <- df %>% 
  drop_na(station)

## Save for later use
write_feather(df, "data/clean/cops.feather")

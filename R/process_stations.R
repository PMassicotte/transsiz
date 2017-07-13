# https://www.pangaea.de/expeditions/cr.php/Polarstern

## Download master track data

# http://hs.pangaea.de/nav/mastertrack/ps/PS92_mastertrack.zip

rm(list = ls())

stations <- read_csv("data/raw/Station_book_24th_June.csv") %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty_cols() %>% 
  janitor::remove_empty_rows() %>% 
  mutate(positionlat = gsub("[[:space:]]", "", positionlat)) %>%
  mutate(positionlat = gsub("°|E|N|'", " ", positionlat)) %>% 
  mutate(positionlat = stringr::str_trim(positionlat)) %>% 
  mutate(latitude = measurements::conv_unit(positionlat, from = 'deg_dec_min', to = 'dec_deg')) %>% 
  mutate(positionlon = gsub("[[:space:]]", "", positionlon)) %>%
  mutate(positionlon = gsub("°|E|N|'", " ", positionlon)) %>% 
  mutate(positionlon = stringr::str_trim(positionlon)) %>% 
  mutate(longitude = measurements::conv_unit(positionlon, from = 'deg_dec_min', to = 'dec_deg')) %>% 
  select(-starts_with("position")) %>% 
  mutate_at(c("longitude", "latitude"), parse_number) %>% 
  mutate(date = as.Date(date, format = "%d.%m.%Y")) %>% 
  separate(station, c("cruise", "station"), sep = "/") %>% 
  mutate(station = gsub("^0*", "", station))

## Remove bad coordinates
stations <- stations %>% 
  filter(longitude > 0 & latitude > 0)

## Only keep stations of interest (i.e. where we have PE curves)
to_keep <- c("19-6",
             "27-2",
             "31-2",
             "32-4",
             "39-6",
             "43-4",
             "46-1",
             "47-3")

stations <- stations %>% 
  filter(station %in% to_keep)

write_feather(stations, "data/clean/stations.feather")

## Try to associate 1 sampling day to each station
stations %>% 
  distinct(date, station)
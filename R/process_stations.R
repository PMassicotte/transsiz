rm(list = ls())

## These are the stations based on ROV data file. There was 1 problem with
## station 027-2 which was sampled over 2 days (2015-05-31 and 2015-06-01) with
## the ROV (see graphic). Because there were more measurements done on
## 2015-06-01, this date was associated to the station.

stations <- data_frame(
  station = c(
    "019-6",
    "027-2",
    "031-2",
    "032-4",
    "039-6",
    "043-4",
    "046-1",
    "047-3"
  ),
  date = as.Date(
    c(
      "2015-05-28",
      "2015-06-01",
      "2015-06-04",
      "2015-06-07",
      "2015-06-12",
      "2015-06-15",
      "2015-06-18",
      "2015-06-20"
    )
  )
)

write_csv(stations, "data/clean/stations.csv")

# # https://www.pangaea.de/expeditions/cr.php/Polarstern
# 
# stations <- read_delim("data/raw/PS92.tab", delim = "\t") %>% 
#   janitor::clean_names(case = "old_janitor") %>% 
#   filter(device == "Ice station") %>% 
#   select(
#     station = event_label,
#     date_time_start = date_time,
#     latitude_start = latitude,
#     longitude_start = longitude
#   ) %>% 
#   mutate(station = stringr::str_extract(station, "\\d+-\\d*")) %>% 
#   filter(station %in% c(
#     "019-6",
#     "027-2",
#     "031-2",
#     "032-4",
#     "039-6",
#     "043-4",
#     "046-1",
#     "047-3"
#   )) %>% 
#   arrange(station)
# 
# stations
# 
# write_csv(stations, "data/clean/stations.csv")

# stations <- readxl::read_excel("data/raw/Sampling_Takuvik.xlsx", sheet = 1, skip = 1) %>% 
#   janitor::clean_names(case = "old_janitor") %>% 
#   select(station:longitude, action) %>% 
#   filter(grepl("^ice", action, ignore.case = TRUE)) %>% 
#   # select(-in_situ_incubations) %>% 
#   mutate(station = stringr::str_extract(station, "\\d+-\\d*")) %>% 
#   mutate_at(c("latitude", "longitude"), gsub, pattern = ",", replacement = ".")  %>% 
#   mutate_at(c("latitude", "longitude"), gsub, pattern = " ", replacement = "") %>%  
#   mutate_at(c("latitude", "longitude"), gsub, pattern = "°", replacement = " ") %>% 
#   mutate_at(c("latitude", "longitude"), gsub, pattern = "'E|'N", replacement = "") %>% 
#   mutate_at(c("latitude", "longitude"), measurements::conv_unit,  "deg_dec_min", "dec_deg") %>% 
#   mutate_at(c("latitude", "longitude"), parse_number) %>% 
#   mutate(hour = format(heure, "%H:%M")) %>% 
#   select(-heure)
# 
# stations
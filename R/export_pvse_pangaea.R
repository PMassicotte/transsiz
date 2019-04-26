rm(list = ls())

stations <- readxl::read_excel("data/raw/station_cast_date_pvse.xls") %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(station = gsub("PS92/", "", station)) %>% 
  separate(station, into = c("station", "cast"), convert = TRUE)

df <- read_csv("data/clean/photosynthetic_parameters.csv") %>% 
  filter(sheet == "water") %>% 
  select(-ends_with("_b"), -rss, -sheet) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate(depth = parse_number(depth))

res <- left_join(df, stations)

write_csv(res, "data/clean/pvse_pangaea.csv")

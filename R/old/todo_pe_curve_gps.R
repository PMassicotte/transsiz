pe <- readxl::read_excel("data/raw/Sampling_Takuvik.xlsx", skip = 1) %>% 
  janitor::clean_names(case = "old_janitor") %>% 
  select(station:longitude) %>% 
  mutate(latitude = gsub("[[:space:]]", "", latitude)) %>%
  mutate(latitude = gsub("°|E|N|'", " ", latitude)) %>% 
  mutate(latitude = stringr::str_trim(latitude)) %>% 
  mutate(latitude = gsub(",", ".", latitude)) %>% 
  mutate(latitude = measurements::conv_unit(latitude, from = 'deg_dec_min', to = 'dec_deg')) %>% 
  mutate(longitude = gsub("[[:space:]]", "", longitude)) %>%
  mutate(longitude = gsub("°|E|N|'", " ", longitude)) %>% 
  mutate(longitude = stringr::str_trim(longitude)) %>% 
  mutate(longitude = gsub(",", ".", longitude)) %>% 
  mutate(longitude = measurements::conv_unit(longitude, from = 'deg_dec_min', to = 'dec_deg')) %>% 
  separate(station, into = c("cruise", "station"), sep = "/") %>% 
  mutate(station = gsub("^0*", "", station)) %>% 
  mutate_at(c("longitude", "latitude"), parse_number)

## Only keep stations of interest (i.e. where we have PE curves)
to_keep <- c("19-6",
             "27-2",
             "31-2",
             "32-4",
             "39-6",
             "43-4",
             "46-1",
             "47-3")

pe <- pe %>% 
  filter(station %in% to_keep)

p1 <- p1 +
  geom_point(data = pe, aes(x = longitude, y = latitude, shape = "PE"), inherit.aes = FALSE, color = "red")

## On dirait que la station la plus à l'Est de chaque série de stations a été utilisée pour faire les courves PE
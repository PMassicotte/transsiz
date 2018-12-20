
read_ice_snow_thickness <- function(file) {

  station_correspondance <- c(
    "1" = "19",
    "2" = "27",
    "3" = "31",
    "4" = "32",
    "5" = "39",
    "6" = "43",
    "7" = "46",
    "8" = "47"
  )
    
  # file <- "data/raw/ice_snow_thickness/TRANSSIZ_GEM_xls/IS1_allData.csv"
  
  df <- fread(file) %>% 
    janitor::clean_names() %>% 
    select(-v1) %>% 
    mutate(id = str_match(file, "IS(\\d+)")[, 2]) %>% 
    mutate(station = station_correspondance[id]) %>% 
    select(-id) %>% 
    mutate(station = parse_number(station)) %>% 
    as_tibble()
  
  
  return(df)
  
}

files <- list.files("data/raw/ice_snow_thickness/TRANSSIZ_GEM_xls/", pattern = "all", full.names = TRUE)

df <- map_df(files, read_ice_snow_thickness)

# Remove negative thickness -----------------------------------------------

# df %>% 
  # filter_at(.vars = vars(starts_with("z")), any_vars(. <= 0))

# df <- df %>% 
#   filter(z_total > z_snow)

# Save --------------------------------------------------------------------

write_csv(df, "data/clean/rov_ice_snow_thickness.csv")
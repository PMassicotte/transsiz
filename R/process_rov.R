# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Process the RAW ROV data.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

start_coords <- function(file) {

  # dat2 <- dat[[1]]  
  
  dat <- read_lines(file, n_max = 10)
  
  latitude <- stringr::str_match(dat, "LATITUDE\\W+(\\d+.\\d+)")[, 2] %>% 
    na.omit() %>% 
    unique() %>% 
    as.numeric()
  
  longitude <- stringr::str_match(dat, "LONGITUDE\\W+(\\d+.\\d+)")[, 2] %>% 
    na.omit() %>% 
    unique() %>% 
    as.numeric()
  
  df <- data_frame(
    longitude,
    latitude
  )
  
  return(df)
  
}

files <- list.files("data/raw/Katlein-etal_2016/datasets/", "rovT_irrad", full.names = TRUE)

dat <- map(files, start_coords) %>% 
  set_names(basename(files)) %>% 
  bind_rows(.id = "file")

write_feather(dat, "data/clean/rov_deployment_positions.feather")
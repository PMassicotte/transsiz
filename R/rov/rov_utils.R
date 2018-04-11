# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Reading functions for the ROV.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

read_transmittance <- function(file) {

  ## Read the starting coord

  dat <- read_lines(file, n_max = 10)

  latitude <- stringr::str_match(dat, "LATITUDE\\W+(\\d+.\\d+)")[, 2] %>%
    na.omit() %>%
    unique() %>%
    as.numeric()

  longitude <- stringr::str_match(dat, "LONGITUDE\\W+(\\d+.\\d+)")[, 2] %>%
    na.omit() %>%
    unique() %>%
    as.numeric()

  ## Read transmittance data

  trans <-
    read_delim(
      file,
      delim = "\t",
      skip = 647,
      col_types = cols(
        .default = col_double(),
        `Date/Time` = col_datetime(format = "")
      )
    ) %>%
    select(1:5) %>%
    janitor::clean_names(case = "old_janitor") %>%
    distinct() %>%
    mutate(starting_longitude = longitude) %>%
    mutate(starting_latitude = latitude) %>%
    mutate(filename = basename(file)) %>%
    mutate(station = stringr::str_extract(filename, "(\\d{3}-\\d+)")) %>%
    mutate(date = as.Date(date_time)) %>%
    drop_na(transmittance_percent) %>% 
    mutate(transmittance = transmittance_percent / 100) %>% 
    select(-transmittance_percent)

  return(trans)
}

read_irradiance <- function(file) {
  irradiance <-
    read_delim(
      file,
      delim = "\t",
      skip = 647,
      col_types = cols(
        .default = col_double(),
        `Date/Time` = col_datetime(format = "")
      )
    ) %>%
    janitor::clean_names(case = "old_janitor") %>%
    distinct() %>% ## Don't know why, but there are duplicated lines in irradiance data
    select(-ed_w_m_2) %>%
    gather(wavelength, irradiance_w_m2_nm, contains("ed")) %>%
    mutate(wavelength = stringr::str_match(wavelength, "ed_(\\d{3})")[, 2]) %>%
    mutate(wavelength = parse_number(wavelength)) %>%
    mutate(filename = basename(file)) %>%
    mutate(station = stringr::str_extract(filename, "(\\d{3}-\\d+)")) %>%
    mutate(date = as.Date(date_time))

  return(irradiance)
}


## Read data
read_depth <- function(file) {
  df <- read_delim(file, skip = 19, delim = "\t") %>%
    janitor::clean_names(case = "old_janitor") %>%
    mutate(filename = basename(file)) %>%
    mutate(station = stringr::str_extract(filename, "(\\d{3}-\\d+)"))

  return(df)
}

## Interpolate the distance to ice bottom. For this, we use the data in another file (depth) and interpolate the rov data from this.

interpol_depth <- function(rov, depth) {
  
  station <- unique(rov$station)

  depth <- depth %>%
    filter(.$station == !!station)

  ## Linear interpolation
  sf <- approxfun(depth$date_time, depth$dist_sea_ice_bottom_m)

  ## Add interpolated data
  return(sf(rov$date_time))
}

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Map of the sampled ice sites. Note that the IBCAO bathymetry has been
# reprojected using QGIS.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

library(ggspatial)

rm(list = ls())

pyrano <- data.table::fread("data/raw/PS92_cont_surf_Pyrano.txt") %>%
  setNames(iconv(names(.), "latin1", "utf-8", sub = "byte")) %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  dplyr::select(c(1, 2, 3, 7)) %>%
  mutate(date_time = anytime::anytime(date_time)) %>%
  mutate(date = lubridate::date(date_time)) %>%
  mutate(date_numeric = lubridate::hour(date_time) * 3600 + lubridate::minute(date_time) * 60 + lubridate::second(date_time)) %>%
  mutate(hour = lubridate::hour(date_time)) %>%
  mutate(minute = lubridate::minute(date_time)) %>%
  filter(par_just_below_surface_µmol >= 0) %>% 
  dplyr::select(date, longitude_e, latitude_n, date) 

stations <- read_csv("data/clean/stations.csv")

proj <- 4326
# proj <- 3413

pyrano <- pyrano %>%
  inner_join(stations, by = "date") %>%
  separate(station, into = c("station", "cast"), convert = TRUE)

pyrano <- pyrano %>% 
  st_as_sf(coords = c("longitude_e", "latitude_n"), crs = 4326) 

ocean <- rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf")

ocean <- ocean %>% 
  st_transform(crs = proj) %>% 
  st_simplify()

pyrano <- pyrano %>% 
  st_transform(crs = proj)

pyrano <- cbind(pyrano, st_coordinates(pyrano))

p <- ggplot() +
  geom_sf(data = ocean, size = 0.25) +
  geom_point(data = pyrano, aes(x = X, y = Y, color = factor(station))) +
  coord_sf(xlim = c(0, 35), ylim = c(76, 83), crs = 4326) +
  scale_x_continuous(breaks = seq(-90, 90, by = 5), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(-180, 180, by = 1), expand = c(0, 0)) +
  theme(panel.grid.major = element_line(size = 0.05, color = "gray90")) +
  labs(color = "Stations") +
  theme(axis.title = element_blank()) +
  scale_color_brewer(palette = "Dark2", direction = 1) +
  annotate(geom = "text", x = 1, y = 78.1, label = "Greenland Sea", vjust = 0, hjust = 0, size = 4, family = "Poppins") +
  annotate(geom = "text", x = 25, y = 76.5, label = "Barents Sea", vjust = 0, hjust = 0, size = 4, family = "Poppins") +
  annotation_north_arrow(location = "tr", which_north = "grid", height = unit(0.75, "cm"), width = unit(0.75, "cm")) +
  annotation_scale(location = "bl")

ggsave("graphs/fig1.pdf", device = cairo_pdf, width = 6, height = 5)
# system("pdfcrop graphs/fig1.pdf graphs/fig1.pdf")

# lim <- tibble(xlim = c(5, 26), ylim = c(76, 82)) %>% 
#   st_as_sf(coords = c("xlim", "ylim"), crs = 4326) %>% 
#   st_transform(crs = proj)
# 
# 
# bathy <- raster::brick("data/raw/IBCAO_V3_500m_RR2.tif") %>%
#   raster::crop(c(0, 2000000, -1707825, -100000)) %>%
#   # raster::sampleRegular(size = 6e5, asRaster = TRUE) %>%
#   raster::projectRaster(crs = "+proj=longlat +datum=WGS84 +no_defs")
# 
# pp <- RStoolbox::ggRGB(bathy, r = 1, g = 2, b = 3, ggLayer = TRUE)
# 
# p <- ggplot() +
#   pp + 
#   # geom_sf(data = ocean, size = 0.25) +
#   geom_point(data = pyrano, aes(x = X, y = Y, color = factor(station))) +
#   coord_sf(xlim = c(0, 35), ylim = c(76, 83), crs = 4326) +
#   scale_x_continuous(breaks = seq(-90, 90, by = 5), expand = c(0, 0)) +
#   scale_y_continuous(breaks = seq(-180, 180, by = 1), expand = c(0, 0)) +
#   theme(panel.grid.major = element_line(size = 0.05, color = "gray90")) +
#   labs(color = "Stations") +
#   theme(axis.title = element_blank()) +
#   scale_color_brewer(palette = "Dark2", direction = 1)




## Add ship path?

# pyrano <- data.table::fread("data/raw/PS92_cont_surf_Pyrano.txt") %>%
#   setNames(iconv(names(.), "latin1", "utf-8", sub = "byte")) %>%
#   janitor::clean_names(case = "old_janitor") %>%
#   as_tibble() %>% 
#   filter(latitude_n > 81) %>%
#   st_as_sf(coords = c("longitude_e", "latitude_n"), crs = 4326) %>% 
#   st_transform(crs = 3411)
# 
# pyrano <- cbind(pyrano, st_coordinates(pyrano))
# 
# p + 
#   geom_path(data = pyrano, aes(x = X, y = Y))

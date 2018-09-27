# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Map of the sampled ice sites. Note that the IBCAO bathymetry has been
# reprojected using QGIS.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

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

pyrano <- pyrano %>%
  inner_join(stations, by = "date") %>%
  separate(station, into = c("station", "cast"), convert = TRUE)

pyrano <- pyrano %>% 
  st_as_sf(coords = c("longitude_e", "latitude_n"), crs = 4326) 

ocean <- rnaturalearth::ne_download(scale = 50, type = 'land', category = 'physical', returnclass = "sf")

ocean <- ocean %>% 
  st_transform(crs = 3411) %>% 
  st_simplify()

pyrano <- pyrano %>% 
  st_transform(crs = 3411)

pyrano <- cbind(pyrano, st_coordinates(pyrano))
  
lim <- tibble(xlim = c(5, 26), ylim = c(76, 82)) %>% 
  st_as_sf(coords = c("xlim", "ylim"), crs = 4326) %>% 
  st_transform(crs = 3411)


bathy <- raster::brick("data/raw/IBCAO_V3_500m_RR2.tif") %>%
  raster::crop(c(600701.3, 1567375.5, -859544.4, -282590.1)) %>% 
  raster::sampleRegular(size = 5e5, asRaster = TRUE)

pp <- RStoolbox::ggRGB(bathy, r = 1, g = 2, b = 3, ggLayer = TRUE)

p <- ggplot() +
  pp + 
  # geom_sf(data = ocean, size = 0.25) +
  geom_path(data = pyrano, aes(x = X, y = Y, color = factor(station)), size = 2) +
  coord_sf(xlim = c(600701.3, 1567375.5), c(-859544.4, -282590.1)) +
  scale_x_continuous(breaks = seq(-90, 90, by = 4), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(-180, 180, by = 1), expand = c(0, 0)) +
  theme(panel.grid.major = element_line(size = 0.05, color = "gray90")) +
  labs(color = "Stations") +
  theme(axis.title = element_blank()) +
  scale_color_brewer(palette = "Set1")

ggsave("graphs/appendix_3.pdf", device = cairo_pdf)
system("pdfcrop graphs/appendix_3.pdf graphs/appendix_1.pdf")

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:
#
# Plot a map with the geographic positions of SUIT and ROV measurements.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

rov <- read_feather("data/clean/rov_transmittance.feather")
suit <- read_feather("data/clean/suit_transmittance.feather")

unique(rov$station)
unique(suit$station_id)

rov <- rov %>%
  separate(station, into = c("station", "cast"), convert = TRUE)

suit <- suit %>%
  separate(station_id, into = c("station", "cast"), sep = 2, convert = TRUE)

wm <- rworldmap::getMap()

p <- ggplot() +
  # geom_polygon(data = wm, aes(x = long, y = lat, group = group)) +
  geom_point(data = rov, aes(x = starting_longitude, y = starting_latitude, color = "ROV")) +
  geom_point(data = suit, aes(x = lon, y = lat, color = "suit")) +
  facet_wrap(~ station, scales = "free") +
  xlab("Longitude") +
  ylab("Latitude") +
  labs(title = "Geographic positions of SUIT and ROV measurements")

ggsave("graphs/map_rov_suit.pdf", device = cairo_pdf, width = 12)

# hrbrthemes::gg_check(p)

# Distance between ROV and SUIT -------------------------------------------

suit <- st_as_sf(suit, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 3411)

rov <- rov %>%
  distinct(starting_longitude, starting_latitude, .keep_all = TRUE)

rov <- st_as_sf(rov, coords = c("starting_longitude", "starting_latitude"), crs = 4326) %>%
  st_transform(crs = 3411)

dist <- st_distance(filter(suit, station == 19), filter(rov, station == 19))

colMeans(dist)

st_write(filter(suit, station == 19), "/home/pmassicotte/Desktop/suit.kml", delete_layer = TRUE)
st_write(filter(rov, station == 19), "/home/pmassicotte/Desktop/rov.kml", delete_layer = TRUE)


## Log of the sampled stations
stations <- read_feather("data/clean/stations.feather")

## Check station positions

## Pyrano data
pyrano <- read_feather("data/clean/pyranometer.feather") %>% 
  arrange(date_time)

wm <- rworldmap::getMap()

p1 <- wm %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon() +
  geom_path(
    data = pyrano,
    aes(x = longitude_e, y = latitude_n, linetype = "Pyrano"),
    inherit.aes = FALSE,
    size = 0.25,
    color = "gray80"
  ) +
  geom_point(
    data = stations,
    aes(x = longitude, y = latitude, color = station),
    inherit.aes = FALSE
  ) +
  coord_map(xlim = c(6, 22),
            ylim = c(81, 82.5),
            projection = "stereo") +
  ggrepel::geom_text_repel(
    data = stations,
    aes(x = longitude, y = latitude, label = as.character(date)),
    inherit.aes = FALSE,
    size = 2,
    segment.size = 0.25,
    color = "gray50"
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  scale_color_brewer(palette = "Dark2") +
  labs(caption = "Ofifcial stations from: Station_book_24th_June.csv\nSuit data from: SUIT_summary.xlsx") +
  labs(color = "Official stations") +
  labs(shape = "Gear") +
  labs(linetype = "Pyranometer") +
  guides(shape = guide_legend(override.aes = list(size = 2))) +
  guides(color = guide_legend(override.aes = list(size = 2))) +
  guides(linetype = guide_legend(override.aes = list(size = 1)))

## C-OPS data
cops <- read_feather("data/clean/cops.feather") %>% 
  distinct(station, longitude, latitude)

p1 <- p1 +
  geom_point(
    data = cops,
    aes(x = longitude, y = latitude, shape = "cops"),
    inherit.aes = FALSE,
    size = 0.75
  )

## Data for the SUIT positions in Station_book_24th_June.csv and
## SUIT_summary.xlsx seems to fit (i.e. same coordinates).

suit <-
  readxl::read_excel("/media/work/Dropbox/TRANSSIZ_2015/SUIT/SUIT_summary.xlsx") %>%
  janitor::clean_names() %>%
  select(station,
         date_time,
         longitude = lon,
         latitude = lat)

p1 <- p1 +
  geom_point(
    data = suit,
    aes(x = longitude, y = latitude, shape = "suit"),
    inherit.aes = FALSE,
    size = 0.75
  )

## ROV

rov <- read_feather("data/clean/rov_deployment_positions.feather")

p1 <- p1 +
  geom_point(
    data = suit,
    aes(x = longitude, y = latitude, shape = "rov"),
    inherit.aes = FALSE,
    size = 0.75
  )

ggsave("graphs/stations.pdf", height = 10, width = 10)



# https://gis.stackexchange.com/questions/2951/algorithm-for-offsetting-a-latitude-longitude-by-some-amount-of-meters
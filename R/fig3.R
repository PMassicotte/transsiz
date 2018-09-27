# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Figure comparing mean vertical profils of primary production.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

geomean <- function(x, na.rm = FALSE, trim = 0, ...)
{
  exp(mean(log(x, ...), na.rm = na.rm, trim = trim, ...))
}

geosd <- function(x, na.rm = FALSE, ...)
{
  exp(sd(log(x, ...), na.rm = na.rm, ...))
}

# PAR data ----------------------------------------------------------------

## Read PAR derived from both SUIT and ROV devices

suit <- read_feather("data/clean/suit_propagated_par_water_column.feather") %>%
  mutate(data_source = "suit")

suit_no_lead <- read_feather("data/clean/suit_propagated_par_water_column.feather") %>% 
  mutate(data_source = "suit_no_lead") %>% 
  filter(transmittance_ed0 <= 0.1)

rov <- read_feather("data/clean/rov_propagated_par_water_column.feather") %>%
  mutate(data_source = "rov")

df <- bind_rows(suit, rov, suit_no_lead) %>%
  select(
    id,
    data_source,
    station,
    cast,
    hour,
    depth,
    par_z_variable_transmittance,
    par_z_100_percent_transmittance
  )

# %>%
#   filter(station != 32) ## Station 32 is discarded because there are no matching ROV-SUIT pairs.

## At the end, shall we use averaged PAR profiles? If so we will loose
## information on the variability induced by transmittance measurements.

## Attach station and cast to the PvsE data
pvse <- read_csv("data/clean/pvse_propagated_parameters.csv")

df <- df %>% 
  left_join(pvse, by = c("station", "depth")) %>% 
  select(-contains("cast"))

## Daily integration
# df %>%
#   mutate(pp_under_ice = ps * (1 - exp(-alpha * par_z_variable_transmittance / ps))) %>%
#   mutate(pp_open_water = ps * (1 - exp(-alpha * par_z_100_percent_transmittance / ps))) %>%
#   group_by(data_source, station, depth) %>%
#   nest() %>%
#   mutate(depth_integrated_pp_under_ice = map_dbl(data, ~pracma::trapz(.$hour, .$pp_under_ice))) %>%
#   mutate(depth_integrated_pp_open_water = map_dbl(data, ~pracma::trapz(.$hour, .$pp_open_water)))

## Daily integration at each depth
res <- df %>%
  filter(station %in% c(19, 27, 31, 39, 43, 46, 47)) %>%
  mutate(pp_under_ice = ps * (1 - exp(-alpha * par_z_variable_transmittance / ps))) %>%
  mutate(pp_open_water = ps * (1 - exp(-alpha * par_z_100_percent_transmittance / ps))) %>%
  group_by(data_source, station, depth, hour) %>%
  summarise(
    mean_pp_under_ice = mean(pp_under_ice),
    mean_pp_open_water = mean(pp_open_water),
    sd_pp_under_ice = sd(pp_under_ice)
  ) %>% 
  group_by(data_source, station, depth) %>% 
  summarise(pp_under_ice = pracma::trapz(hour, mean_pp_under_ice),
            pp_open_water = pracma::trapz(hour, mean_pp_open_water)) %>% 
  ungroup()

res

sic <- read_csv("data/clean/sic.csv") %>% 
  select(-sd)

res <- res %>% 
  left_join(sic) %>% 
  mutate(
    pp_mixing_model = ((sic_9) * pp_under_ice) + ((1 - sic_9) * pp_open_water)
  ) 

## Remove open water pp, we do not need it now
res <- res %>% 
  select(-pp_open_water)

# Remove non-needed data --------------------------------------------------

df <- res %>% 
  gather(pp_source, pp, starts_with("pp_"))

## For the SUIT data, we only want to keep:
## 1) the mixing model made with transmittance <= 0.1
## 2) the pp underice made with all suit data (not with T <= 0.1)

df <- df %>% 
  filter(!(data_source == "suit" & pp_source == "pp_mixing_model")) %>% 
  filter(!(data_source == "suit_no_lead" & pp_source == "pp_under_ice")) %>% 
  mutate(data_source = if_else(str_detect(data_source, "suit"), "suit", data_source))

# In-situ PP (Jean-Eric) --------------------------------------------------

jet <-
  read_excel(
    "data/raw/TRANSSIZ_PP.xlsx",
    col_names = c("station", "ctd", "depth", "data_source", "pp"),
    range = cell_limits(c(3,1), c(NA, 5))
  ) %>% 
  mutate(data_source = str_to_lower(data_source)) %>% 
  filter(data_source == "in situ") %>% 
  select(station, depth, data_source, pp) %>% 
  mutate(pp_source = "in situ") %>% 
  filter(station %in% c(19, 27, 31, 39, 43, 46, 47)) 

jet

res <- bind_rows(df, jet)


# Plot --------------------------------------------------------------------

res %>% 
  ggplot(aes(x = pp, y = depth, color = interaction(pp_source, data_source))) +
  geom_path() +
  scale_y_reverse() +
  facet_wrap(~station, scales = "free") +
  xlab(bquote("Primary production" ~(mgC%*%m^{-3}%*%d^{-1}))) +
  ylab("Depth (m)") +
  theme(legend.title = element_blank())

ggsave("graphs/fig3.pdf", device = cairo_pdf, width = 7, height = 6.22 * 0.75)
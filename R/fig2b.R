# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Figure comparing mean vertical profils of primary production.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())

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

df <- df %>% 
  group_by(data_source, station, depth) %>% 
  nest() %>% 
  mutate(id = map(data, function(x) { # Create an id that represents every profiles
    
    x %>% 
      mutate(id = rep(1:(nrow(.) / 24), each = 24))
    
  })) %>% 
  unnest(id)

## Daily integration
res <- df %>%
  mutate(pp_under_ice = ps * (1 - exp(-alpha * par_z_variable_transmittance / ps))) %>%
  mutate(pp_open_water = ps * (1 - exp(-alpha * par_z_100_percent_transmittance / ps))) %>%
  group_by(data_source, station, id, depth) %>%
  nest() %>%
  mutate(pp_under_ice = map_dbl(data, ~pracma::trapz(.$hour, .$pp_under_ice))) %>%
  mutate(pp_open_water = map_dbl(data, ~pracma::trapz(.$hour, .$pp_open_water)))

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
  tidyr::gather(pp_source, pp, starts_with("pp_"))

## For the SUIT data, we only want to keep:
## 1) the mixing model made with transmittance <= 0.1
## 2) the pp underice made with all suit data (not with T <= 0.1)

df <- df %>% 
  filter(!(data_source == "suit" & pp_source == "pp_mixing_model")) %>% 
  filter(!(data_source == "suit_no_lead" & pp_source == "pp_under_ice")) %>% 
  mutate(data_source = if_else(str_detect(data_source, "suit"), "suit", data_source))

# Plot --------------------------------------------------------------------

df %>% 
  ggplot(aes(x = pp, y = depth, color = interaction(pp_source, data_source), group = interaction(data_source, station, id, pp_source))) +
  geom_path() +
  scale_y_reverse() +
  facet_grid(pp_source~station, scales = "free") +
  xlab(bquote("Primary production" ~(mgC%*%m^{-3}%*%d^{-1}))) +
  ylab("Depth (m)") +
  theme(legend.title = element_blank())

ggsave("graphs/fig3.pdf", device = cairo_pdf, width = 7, height = 6.22 * 0.75)

# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Primary production calculated from suit rov transmittances.
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

df <- bind_rows(suit, suit_no_lead, rov) %>%
  select(
    id,
    data_source,
    station,
    cast,
    hour,
    depth,
    par_z_variable_transmittance,
    par_z_100_percent_transmittance
  ) %>%
  filter(station != 32) ## Station 32 is discarded because there are no matching ROV-SUIT pairs.

## At the end, shall we use averaged PAR profiles? If so we will loose
## information on the variability induced by transmittance measurements.

## Attach station and cast to the PvsE data
pvse <- read_csv("data/clean/pvse_propagated_parameters.csv")

df <- df %>% 
  left_join(pvse, by = c("station", "depth")) %>% 
  select(-contains("cast"))

# Calulate PP -------------------------------------------------------------

# df <- df %>% 
#   filter(data_source == "suit")

res <- df %>%
  mutate(pp_under_ice = ps * (1 - exp(-alpha * par_z_variable_transmittance / ps))) %>%
  mutate(pp_open_water = ps * (1 - exp(-alpha * par_z_100_percent_transmittance / ps))) %>%
  group_by(id, data_source, station, hour) %>%
  nest() %>%
  mutate(depth_integrated_pp_under_ice = map_dbl(data, ~ pracma::trapz(.$depth, .$pp_under_ice))) %>%
  mutate(depth_integrated_pp_open_water = map_dbl(data, ~ pracma::trapz(.$depth, .$pp_open_water))) %>%
  mutate(id = rep(1:((nrow(.)) / 24), each = 24)) %>% 
  group_by(id, station, data_source) %>% 
  nest() %>% 
  mutate(daily_integrated_pp_under_ice = map_dbl(data, ~pracma::trapz(.$hour, .$depth_integrated_pp_under_ice))) %>% 
  mutate(daily_integrated_pp_open_water = map_dbl(data, ~pracma::trapz(.$hour, .$depth_integrated_pp_open_water))) %>% 
  select(-data)

# SIC ---------------------------------------------------------------------

## Match with SIC data (used to the mixing model)

sic <- read_csv("data/clean/sic.csv") %>% 
  select(-sd)

res  <- res %>% 
  left_join(sic)


# Mixing model ------------------------------------------------------------

res <- res %>%
  mutate(
    daily_integrated_pp_mixing_model = ((sic_9) * daily_integrated_pp_under_ice) + ((1 - sic_9) * daily_integrated_pp_open_water)
  )

## Remove open water pp, we do not need it now
res <- res %>% 
  select(-daily_integrated_pp_open_water)


# Remove non-needed data --------------------------------------------------

df <- res %>% 
  tidyr::gather(pp_source, pp, starts_with("daily"))

## For the SUIT data, we only want to keep:
## 1) the mixing model made with transmittance <= 0.1
## 2) the pp underice made with all suit data (not with T <= 0.1)

df <- df %>% 
  filter(!(data_source == "suit" & pp_source == "daily_integrated_pp_mixing_model")) %>% 
  filter(!(data_source == "suit_no_lead" & pp_source == "daily_integrated_pp_under_ice")) %>% 
  mutate(data_source = if_else(str_detect(data_source, "suit"), "suit", data_source))

# Plot --------------------------------------------------------------------

counts <- df %>% 
  group_by(station, data_source, pp_source) %>% 
  tally() %>% 
  ungroup()

plain <- function(x, ...) {
  format(x, ..., scientific = FALSE, drop0trailing = TRUE)
}

df_mean <- df %>% 
  group_by(station, data_source, pp_source) %>% 
  summarise(mean_pp = mean(pp)) %>%
  ungroup() %>% 
  mutate(data_source = str_to_title(data_source))

p <- df %>% 
  mutate(data_source = str_to_title(data_source)) %>% 
  ggplot(aes(x = data_source, y = pp, fill = pp_source)) +
  geom_violin(scale = "width", size = 0.25) +
  facet_wrap(~station) +
  scale_y_log10(labels = plain) +
  annotation_logticks(sides = "l", size = 0.25) +
  theme(legend.title = element_blank()) +
  theme(axis.title.x = element_blank()) +
  # scale_fill_discrete(labels = c("daily_integrated_pp_under_ice" = "Under ice", "daily_integrated_pp_mixing_model" = "Mixing model")) +
  theme(legend.justification = c(0, 0), legend.position = c(0.35, 0.1)) +
  ylab(bquote("Primary production" ~(mgC~m^{-2}~d^{-1})))  +
  stat_summary(
    fun.y = mean,
    geom = "point",
    aes(group = pp_source),
    position = position_dodge(.9),
    color = "black",
    size = 1,
    show.legend = FALSE
  ) +
  scale_fill_manual(
    breaks = c("daily_integrated_pp_under_ice", "daily_integrated_pp_mixing_model"),
    values = RColorBrewer::brewer.pal(3, "Set1"),
    labels = c(
      bquote(PP[Underice]),
      bquote(PP[Mixing])
    )
  )

ggsave("graphs/fig4.pdf", device = cairo_pdf, height = 190 / 1.61803398875, width = 190, units = "mm")

write_feather(df, "data/clean/primary_production_rov_vs_suit.feather")
write_csv(df, "data/clean/primary_production_rov_vs_suit.csv")

# Stats -------------------------------------------------------------------

round(range(df$pp), digits = 3)

df %>% 
  group_by(station, data_source, pp_source) %>% 
  summarise(mean_pp = mean(pp)) %>% 
  arrange(desc(mean_pp))


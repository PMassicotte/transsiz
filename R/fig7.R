rm(list = ls())

options(future.globals.maxSize = 4294967296 * 3)

df <- read_feather("data/clean/primary_production_rov_vs_suit.feather")

res <- df %>% 
  group_by(station, data_source, pp_source) %>% 
  nest() %>% 
  mutate(average_pp = map_dbl(data, ~mean(.$pp))) %>% 
  crossing(n_random_sample = 1:500, simul_id = 1:100) %>% 
  mutate(random_average_pp = future_map2_dbl(data, n_random_sample, function(data, n) {

    data %>% 
      sample_n(n, replace = TRUE) %>% 
      summarise(mean_pp = mean(pp)) %>% 
      pull(mean_pp)
    
  }, .progress = TRUE, .options = future_options(seed = .Random.seed[10])))

res %>% 
  select(-data) %>% 
  rename(mean_random_average_pp = random_average_pp) %>% 
  write_csv("~/Desktop/data_simulation_spot_measurements.csv")

# res <- res %>% 
#   mutate(abs_error = abs(average_pp - random_average_pp) / (abs(average_pp + random_average_pp) / 2)) 

res2 <- res %>% 
  mutate(abs_error = abs(average_pp - random_average_pp) / (abs(average_pp + random_average_pp) / 2)) %>%
  # mutate(abs_error = abs((average_pp - random_average_pp) / average_pp)) %>% 
  group_by(data_source, pp_source, n_random_sample) %>% 
  summarise(mean_abs_error = mean(abs_error), sd_abs_error = sd(abs_error))

mod <- res2 %>%
  group_by(data_source, pp_source) %>%
  nest() %>%
  mutate(mod = map(
    data,
    ~ nls(
      .$mean_abs_error ~ a * exp(-k * .$n_random_sample) + offset,
      data = .,
      start = list(a = 100, k = 0.1, offset = 0)
    )
  )) %>%
  mutate(pred = map2(data, mod, modelr::add_predictions)) %>%
  unnest(pred)

write_csv(mod, "data/clean/pp_error_as_function_n_observation.csv")

# Figure ------------------------------------------------------------------

mod <- read_csv("data/clean/pp_error_as_function_n_observation.csv")

device_label <- c(
  "suit" = "SUIT",
  "rov" = "ROV"
)

mod %>%
  ungroup() %>% 
  filter(n_random_sample <= 250) %>% 
  mutate(pp_source = ifelse(str_detect(pp_source, "under_ice"), "italic(P)[underice]", "italic(P)[mixing]")) %>% 
  ggplot(aes(x = n_random_sample, y = mean_abs_error)) +
  geom_ribbon(aes(ymin = mean_abs_error - sd_abs_error, ymax = mean_abs_error + sd_abs_error), alpha = 0.5, fill = "gray50") +
  geom_point(size = 1) +
  scale_y_continuous(labels = scales::percent, breaks = seq(-4, 4, by = 0.2)) +
  facet_grid(data_source ~ pp_source, labeller = labeller(data_source = device_label, pp_source = label_parsed), scales = "free") +
  # geom_line(aes(y = pred), color = RColorBrewer::brewer.pal(3, "Set1")[1]) +
  xlab("Number of randomly chosen observations") +
  ylab("Mean relative error (%)") 

ggsave("graphs/fig7.pdf", device = cairo_pdf, height = 190 / 1.61803398875, width = 190, units = "mm")
# system("pdfcrop graphs/fig6.pdf graphs/fig6.pdf")

## Calculate how many observations are needed to have specific mean errors

sf <- mod %>% 
  group_by(data_source = toupper(data_source), pp_source) %>% 
  nest() %>% 
  mutate(sf = map(data, function(data) {
    
    sf <- with(data, approxfun(mean_abs_error, n_random_sample))
    
    tibble(mean_abs_error = c(0.10, 0.15, 0.20, 0.25), n_random_sample = sf(mean_abs_error))
    
  })) %>% 
  unnest(sf)

mean(sf$n_random_sample)

df_table <- sf %>% 
  spread(mean_abs_error, n_random_sample) %>% 
  mutate_if(is.numeric, round) 

df_table <- df_table %>% 
  mutate(pp_type = ifelse(str_detect(pp_source, "mixing"), "mixing", "underice")) %>% 
  mutate(pp_source = sprintf("$PP^{\\mathrm{%s}}_{\\mathrm{%s}}$", data_source, pp_type)) %>% 
  select(-data_source, -pp_type)
  
  # mutate(pp_source = ifelse(str_detect(pp_source, "mixing"), "$PP^{\\mathrm{fr}}_{s\\mathrm{MAX}}$", "PP\\textsubscript{Underice}"))

addtorow <- list()
addtorow$pos <- list(0, 0)
addtorow$command <- c("& \\multicolumn{4}{c}{Relative error threshold} \\\\\n",
                      "Model & 10\\% & 15\\% & 20\\% & 25\\% \\\\\n")
print(
  autoformat(xtable(df_table, caption = "Number of measurements needed to reach various relative error thresholds.")),
  add.to.row = addtorow,
  include.colnames = FALSE,
  include.rownames = FALSE,
  file = "article/jgr/tables/table3.tex",
  sanitize.text.function = function(x){x},
  booktabs = TRUE
)

# %>% 
#   xtable::xtable(caption = "Number of random measurements needed to reach mean absolute errors of 10\\%, 15\\%, 20\\% and 25\\%.") %>% 
#   xtable::autoformat() %>% 
#   print(file = "article/jgr/tables/table4.tex")

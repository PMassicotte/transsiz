
# PS parameters -----------------------------------------------------------

df <- read_feather("data/clean/photosynthetic_parameters.feather")

df <- df %>% 
  gather(parameter, value, ps:pb_max)

df %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~parameter, scales = "free")

ggsave("graphs/histo_ps_params.pdf")

res <- df %>% 
  filter(sheet == "water") %>%
  mutate(depth = parse_number(depth)) %>% 
  # spread(parameter, value) %>% 
  group_by(parameter) %>% 
  nest()

make_plot <- function(df, param) {
  
  p <- df %>% 
    ggplot(aes(x = value, y = depth)) +
    geom_point() +
    geom_path() +
    scale_y_reverse() +
    ylab("Depth (m)") +
    labs(title = param) +
    facet_wrap(~date, scales = "free")

  return(p)
    
}

p <- map2(res$data, res$parameter, make_plot)

pdf("graphs/vertical_profil_params.pdf", width = 10, height = 7)

map(p, print)

dev.off()


# chla --------------------------------------------------------------------

read_p_manip <- function(sheet) {
  
  pp <- readxl::read_excel(
    "data/raw/Data_PvsE_AllStations.xlsx",
    col_types = c("date", "text", "numeric", "numeric", "numeric", "numeric"),
    na = c("", "NaN"),
    sheet = sheet
  ) %>%
    janitor::clean_names(case = "old_janitor") %>%
    fill(date, depth, chloro, dilution) %>%
    drop_na() %>%
    rename(light = light_incub) %>%
    group_by(date, depth) %>%
    mutate(model_type = if_else(p_manip[which.max(light)] < max(p_manip), "model1", "model2")) %>% 
    mutate(p_manip = p_manip * dilution)
  
  return(pp)
  
}

sheets <- readxl::excel_sheets("data/raw/Data_PvsE_AllStations.xlsx")
pp <- map(sheets, read_p_manip) %>% 
  set_names(sheets) %>% 
  bind_rows(.id = "sheet") %>% 
  ungroup() %>% 
  filter(sheet == "water") %>% 
  distinct(sheet, date, depth, chloro) %>% 
  mutate(depth = parse_number(depth))


pp %>% 
  ggplot(aes(x = chloro, y = depth)) +
  geom_point() +
  geom_path() +
  facet_wrap(~date, scales = "free") +
  scale_y_reverse() +
  ylab("Depth (m)") +
  xlab(bquote("Chla"~(mg%*%m^{-3})))

ggsave("graphs/vertical_profil_chla.pdf", width = 10, height = 7)

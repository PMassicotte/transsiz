rm(list = ls())

source("R/zzz.R")

# Primary production data -------------------------------------------------

read_p_manip <- function(sheet) {
  
  pp <- readxl::read_excel(
    "data/raw/Data_PvsE_AllStations.xlsx",
    col_types = c("date", "text", "numeric", "numeric", "numeric", "numeric"),
    na = c("", "NaN"),
    sheet = sheet
  ) %>%
    janitor::clean_names() %>%
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
  ungroup()

# pp %>% 
#   ggplot(aes(x = light, y = p_manip)) +
#   geom_point(aes(color = model_type)) +
#   facet_wrap(~gi, scales = "free")

fit_pe <- function(df, model_type) {
  
  mod <- NA
  
  opt <- nls.control(maxiter = 400, minFactor = 1e-10, tol = 1e-10)
  
  if (model_type == "model1") {
    
    mod <- minpack.lm::nlsLM(
      p_manip ~
        ps * (1 - exp(-alpha * light / ps)) * exp(-beta * light / ps) + p0,
      data = df,
      start = list(
        ps = max(df$p_manip, na.rm = TRUE),
        alpha = 0,
        beta = 0.000001,
        p0 = 0
      ),
      lower = c(
        ps =  min(df$p_manip, na.rm = TRUE),
        alpha = 0,
        beta = 0,
        p0 = -Inf
      ),
      upper = c(
        ps =  10,
        alpha = Inf,
        beta = 0.6,
        p0 = Inf
      ),
      control = opt
    )
    
  } else if (model_type == "model2") {
    
    mod <- minpack.lm::nlsLM(
      p_manip ~
        ps * (1 - exp(-alpha * light / ps)) + p0,
      data = df,
      start = list(
        ps = 2,
        alpha = 0.01,
        p0 = 0.01
      ),
      lower = c(
        ps = min(df$p_manip, na.rm = TRUE),
        alpha = 0,
        p0 = -Inf
      ), 
      upper = c(
        ps = Inf,
        alpha = Inf,
        p0 = Inf
      ),
      control = opt
    )
    
  }
  
  return(mod)
  
}

pp <- pp %>% 
  ungroup() %>% 
  group_by(date, depth, model_type, sheet) %>% 
  nest() %>% 
  mutate(mod = map2(data, model_type, fit_pe)) %>%
  mutate(prediction = map2(data, mod, modelr::add_predictions)) %>% 
  mutate(params = map(mod, broom::tidy)) %>% 
  mutate(metric = map2(mod, data, calculate_metrics))

plot_pe <- function(df, mod, model_type, date, depth, sheet) {
  
  df <- df %>% 
    modelr::add_predictions(., mod) %>% 
    mutate(model_type = model_type)
  
  # mod2 <- phytotools::fitPGH(df$light, df$p_manip)
  # print(mod2)
  
  alpha <- coef(mod)["alpha"]
  p0 <- coef(mod)["p0"]
  ps <- coef(mod)["ps"]
  beta <- coef(mod)["beta"]
  
  p <- df %>% 
    ggplot(aes(x = light, y = p_manip)) +
    geom_point(aes(color = model_type)) +
    geom_line(aes(y = pred)) +
    geom_abline(slope = alpha, intercept = p0, color = "blue") +
    geom_hline(yintercept = p0, color = "blue") +
    # geom_hline(yintercept = ps, color = "red") +
    geom_abline(slope = -beta, intercept = ps + p0, color = "blue") +
    labs(title = paste(date, depth, sheet))
  
  print(p)
  
}

pdf("graphs/pe_curves.pdf", width = 6, height = 4)

pmap(list(pp$data, pp$mod, pp$model_type, as.character(pp$date), pp$depth, pp$sheet),  plot_pe)

dev.off()

metric <- pp %>% 
  unnest(metric) %>% 
  select(date, depth, model_type, sheet, pb_max, alpha_b, beta_b, ek, rss, r2)

write_feather(metric, "data/clean/photosynthetic_parameters.feather")

# write_csv(metric, "data/photosynthetic_parameters.csv")


# Duplicate the first row and assume the same values at depth = 0 m

#################### ATTENTION, DEPTH IS NOT NUMERIC, SEE WITH FLAVIENNE ####################
# params <- params %>%
#   group_by(date, sheet) %>%
#   complete(depth = c(0, unique(depth))) %>%
#   fill(model_type, alpha, beta, p0, ps, .direction = "up")
# 
# # Should be all equal
# params %>% 
#   filter(depth %in% c(0, 1))
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
  ungroup()

pp %>% 
  filter(sheet == "water") %>% 
  group_by(date, depth) %>% 
  summarise(max_light = max(light)) %>% 
  pull(max_light) %>% 
  range()

# pp %>%
#   ggplot(aes(x = light, y = p_manip)) +
#   geom_point(aes(color = model_type)) +
#   facet_wrap(~gi, scales = "free")

fit_pe <- function(df, model_type) {
  
  mod <- NA

  opt <- nls.control(maxiter = 400, minFactor = 1e-10, tol = 1e-10)

  model_type = "model1"
  
     mod <- minpack.lm::nlsLM(
      p_manip ~
      ps * (1 - exp(-alpha * light / ps)) * exp(-beta * light / ps) + p0,
      data = df,
      start = list(
        ps = max(df$p_manip, na.rm = TRUE),
        alpha = 0.001,
        beta = 0.001,
        p0 = 0
      ),
      lower = c(
        ps = max(df$p_manip, na.rm = TRUE) / 2,
        alpha = 0,
        beta = 0,
        p0 = -Inf
      ),
      upper = c(
        ps = max(df$p_manip, na.rm = TRUE) * 2,
        alpha = 1,
        beta = 1,
        p0 = Inf
      ),
      control = opt
    )
  

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
    mutate(model_type = model_type)

  pred <- df %>%
    modelr::data_grid(light = modelr::seq_range(light, 100)) %>%
    modelr::add_predictions(., mod)


  # mod2 <- phytotools::fitPGH(df$light, df$p_manip)
  # print(mod2)

  alpha <- coef(mod)["alpha"]
  p0 <- coef(mod)["p0"]
  ps <- coef(mod)["ps"]
  beta <- coef(mod)["beta"]

  pm <- ps * (alpha / (alpha + beta)) * (beta / (alpha + beta))^(beta / alpha)
  ek <- pm / alpha
  # abline(h = pm + p0)
  
  df$tt = ps*exp(-beta*df$light/ps) + p0

  p <- df %>%
    ggplot(aes(x = light, y = p_manip)) +
    geom_point(aes(color = model_type)) +
    geom_line(data = pred, aes(x = light, y = pred)) +
    geom_abline(slope = alpha, intercept = p0, color = "blue", lwd = 0.25, lty = 2) +
    geom_hline(yintercept = p0, color = "blue", lwd = 0.25, lty = 2) +
    geom_hline(yintercept = pm + p0, color = "blue", lwd = 0.25, lty = 2) +
    geom_vline(xintercept = ek, color = "blue", lwd = 0.25, lty = 2) +
    geom_hline(yintercept = ps +p0, color = "red", lwd = 0.25, lty = 2) +
    # geom_hline(yintercept = ps, color = "red") +
    # geom_abline(slope = -beta, intercept = ps + p0, color = "blue") +
    labs(title = paste(date, depth, sheet)) +
    geom_path(aes(x = light, y =tt ), color = "green")

  print(p)
}

pdf("graphs/pe_curves2.pdf", width = 6, height = 4)

pmap(list(pp$data, pp$mod, pp$model_type, as.character(pp$date), pp$depth, pp$sheet), plot_pe)

dev.off()

metric <- pp %>%
  unnest(metric) %>%
  select(-data, -mod, -prediction, -params)

write_csv(metric, "data/clean/photosynthetic_parameters.csv")

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

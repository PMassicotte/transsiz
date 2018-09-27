# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  
#
# Example of PvsE curve (appendix).
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

rm(list = ls())


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

pp <- pp %>% 
  filter(as.Date(date) == "2015-05-31" & depth == 1)


mod <- minpack.lm::nlsLM(
  p_manip ~
    ps * (1 - exp(-alpha * light / ps)) * exp(-beta * light / ps) + p0,
  data = pp,
  start = list(
    ps = max(pp$p_manip, na.rm = TRUE),
    alpha = 0,
    beta = 0.000001,
    p0 = 0
  ))

pp <- pp %>% 
 modelr::add_predictions(model = mod)

alpha <- coef(mod)["alpha"]
p0 <- coef(mod)["p0"]
ps <- coef(mod)["ps"]
beta <- coef(mod)["beta"]

pm <- ps * (alpha / (alpha + beta)) * (beta / (alpha + beta))^(beta / alpha)
ek <- pm / alpha

pp %>% 
  ggplot(aes(x = light, y = p_manip)) +
  geom_point() +
  geom_line(aes(y = pred), color = "red") +
  xlab(bquote(PAR~(mu*mol%*%m^{-2}%*%s^{-1}))) +
  ylab("Primary production") +
  geom_abline(slope = alpha, intercept = p0, color = "blue", lwd = 0.25, lty = 2) +
  geom_hline(yintercept = p0, color = "blue", lwd = 0.25, lty = 2) +
  geom_hline(yintercept = pm + p0, color = "blue", lwd = 0.25, lty = 2) +
  geom_vline(xintercept = ek, color = "blue", lwd = 0.25, lty = 2)

ggsave("graphs/appendix_4.pdf", device = cairo_pdf, width = 8.7 * 0.75, height = 6.22 * 0.75)

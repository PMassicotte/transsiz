calculate_metrics <- function(mod, data) {
  
  if (is.null(mod)) {
    return(data.frame(
      ps = NA,
      alpha = NA,
      beta = NA,
      p0 = NA,
      pm = NA,
      ek = NA,
      rss = NA,
      r2 = NA
    ))
  }
  
  ps <- coef(mod)["ps"]
  alpha <- coef(mod)["alpha"]
  beta <- coef(mod)["beta"]
  p0 <- coef(mod)["p0"]
  
  rss <- sum(residuals(mod) ^ 2)
  
  ## Calculate the pseudo-R2
  y <- mod$m$lhs()
  yy <- fitted(mod)
  r2 <- 1 - sum((y - yy) ^ 2) / (length(y) * var(y))
  
  ## Extra parameters (from the Excel sheet I was given)
  pm <- ps * (alpha / (alpha + beta)) * (beta / (alpha + beta)) ^ (beta / alpha)
  ek <- pm / alpha
  
  ## Normalize
  alpha_b <- alpha / unique(data$chloro)
  beta_b <- beta / unique(data$chloro)
  pb_max <- pm / unique(data$chloro)
  
  ## Return the data
  df <- data.frame(ps, alpha, beta, p0, pm, ek, alpha_b, beta_b, pb_max, rss, r2)
  
  return(df)
}


# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }


read_data <- function(file) {
  
  trans <- read_delim(file, delim = "\t", skip = 647) %>% 
    select(1:5) %>% 
    janitor::clean_names() 
  
  return(trans)
  
}
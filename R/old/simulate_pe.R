
simul <- function(mod, data, n = 100) {
  set.seed(1234)

  light <- data$light

  simulation <- mvrnorm(n = n, mu = coef(mod), Sigma = vcov(mod), empirical = TRUE) %>%
    data.frame() %>%
    mutate(simul = 1:nrow(.)) %>%
    group_by(simul) %>%
    nest() %>%
    mutate(
      p_manip =
        map(
          data,
          ~ .$ps * (1 - exp(-.$alpha * light / .$ps)) * exp(-.$beta * light / .$ps) + .$p0
        )
    ) %>%
    unnest(data) %>%
    unnest(p_manip) %>%
    mutate(light = rep(light, times = n))

  return(simulation)
}

plot_simulations <- function(df, depth) {
  p <- df %>%
    ggplot(aes(x = light, y = p_manip, group = simul)) +
    geom_line(size = 0.25) +
    labs(
      title = paste("Depth:", depth),
      subtitle = paste("Number of simulation:", length(unique(df$simul)))
    )

  invisible(p)
}

plot_histo <- function(mod) {
  mod %>%
    unnest(simulation) %>%
    gather(parameter, value, ps, alpha, beta, p0) %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 100) +
    facet_grid(depth ~ parameter, scales = "free")
}

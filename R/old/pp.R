# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate PP based on P vs I curves.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

#' Last updated: `r Sys.time()`

#' ## Open the PS file and do some cleaning
ps <- readxl::read_excel("data/Global_PS_for_Takuvik.xlsx") %>%
  janitor::clean_names(case = "old_janitor") %>%
  mutate(time = anytime::anytime(paste(date, format(time, "%H:%M:%S")))) %>%
  mutate(time_numeric = as.numeric(time)) %>%
  mutate(hour = as.numeric(format(time, "%H"))) %>%
  mutate(par_just_below_ice_scalar_µmolquanta_corrected = par_just_below_ice_scalar_µmolquanta)

#' There is a problem with data later than 2015-06-20 20:20:00. Replace these "outliers" with the observations measured at the begining.

i <- which(ps$time >= "2015-06-20 20:20:00")

ps$par_just_below_ice_scalar_µmolquanta_corrected[i] <-
  ps$par_just_below_ice_scalar_µmolquanta_corrected[length(i):1]

#' ## Plot the "raw" data

#' This is the raw data to work with.

ps %>%
  ggplot(aes(x = time, y = par_just_below_ice_scalar_µmolquanta)) +
  geom_point() +
  geom_point(aes(y = par_just_below_ice_scalar_µmolquanta_corrected), col = "red")

#' ## Hourly PAR

res <- ps %>%
  group_by(hour) %>%
  nest() %>%
  mutate(e = map(data, ~ mean(.$par_just_below_ice_scalar_µmolquanta_corrected))) %>%
  unnest(e)

res %>%
  ggplot(aes(x = hour, y = e)) +
  geom_line() +
  geom_point()

#' ## Calculate e at each depth

#' This is done based on the integrated surface PAR and a kd value of 0.15 $m^{-1}$.

compute_ez <- function(e) {
  kd <- -0.15 # m-1

  depth <- c(0, 2.1, 5, 10, 15, 30, 50)
  e_z <- e * exp(kd * depth)

  df <- data_frame(
    depth = depth,
    e_z = e_z
  )

  return(df)
}

insitu_light <- res %>%
  mutate(ez = map(.$e, compute_ez)) %>%
  unnest(ez)

insitu_light %>%
  ggplot(aes(x = e_z, y = depth, group = hour)) +
  geom_line() +
  scale_y_reverse()

#' ## PI curve

#' In this section we start by calculating photosynthetic parameters of one P vs E curve.

# Open de PE data
df <- read_csv("data/pe.csv") %>%
  fill(depth)

# Fit the PE curve for each depth
mod <- df %>%
  group_by(depth) %>%
  nest() %>%
  mutate(model = map(
    data,
    ~ minpack.lm::nlsLM(
      p_manip ~
      ps * (1 - exp(-alpha * light / ps)) * exp(-beta * light / ps) + p0,
      data = .,
      start = list(
        ps = 0.5,
        alpha = 0.005,
        beta = 0.004,
        p0 = 0
      ),
      lower = c(0, 0, 0, -Inf)
    )
  )) %>%
  mutate(fitted = map(model, broom::augment)) %>%
  mutate(coef = map(model, broom::tidy))

# Overview of the PE curves
mod %>%
  unnest(fitted) %>%
  ggplot(aes(x = light, y = p_manip)) +
  geom_point() +
  geom_line(aes(y = .fitted), col = "red") +
  facet_wrap(~ depth, scales = "free")

params <- mod %>%
  unnest(coef) %>%
  dplyr::select(depth, term, estimate) %>%
  spread(term, estimate)

# Duplicate the first row and assume the same value at depth = 0 m

params <- rbind(params[1, ], params)
params$depth[1] <- 0

params

#' ## Calculate hourly PP at each depth

dat <- inner_join(insitu_light, params, by = "depth") %>%
  mutate(p = ps * (1 - exp(-alpha * e_z / ps)) * exp(-beta * e_z / ps))

dat %>%
  ggplot(aes(x = hour, y = p)) +
  geom_point() +
  facet_wrap(~ depth) +
  xlab("Hour of the day") +
  ylab("PP")

#' ## Calculate integrated PP at each depth

#' Calculating the sum of PP at each depth gives us the daily PP at each particular depth.

res <- dat %>%
  group_by(depth) %>%
  summarise(sum_day = sum(p))

res %>%
  ggplot(aes(x = sum_day, y = depth)) +
  geom_point() +
  geom_line() +
  scale_y_reverse()

#' ## Calculate the integrated PP

#' This is the integrated value of PP for 1 day. This is done by calculating the area under the curve of the daily PP at each depth.

int_pp <- pracma::trapz(res$depth, res$sum_day)
int_pp

#' ## Simulations

#' For each P vs E curve, I generate *n* simulated curves based a multivariate normal distribution of the fitted parameters. This will be used to estimate the error on the PP estimation.

mod <- mod %>%
  mutate(simulation = map2(model, data, simul, n = 10000)) # 10 simulations per depth

p <- map2(mod$simulation, mod$depth, plot_simulations)

cowplot::plot_grid(plotlist = p)

#' At this point we have generated 10 000 P vs E curves. Let's have a look to the histogram for each photosynthetic parameter.

plot_histo(mod)

#' This is rassuring to see that they all follow a normal distribution. Now, we are ready to calculate the depth integrated PP.

#' ### Depth integrated PP

depth_pp <- mod %>%
  unnest(simulation) %>%
  left_join(insitu_light, by = "depth") %>%
  distinct(depth, simul, hour, .keep_all = TRUE) %>%
  mutate(p = ps * (1 - exp(-alpha * e_z / ps)) * exp(-beta * e_z / ps)) %>%
  group_by(depth, simul) %>%
  summarise(sum_day = sum(p))

## Assume that the surface values are the same at surface
surface_pp <- depth_pp[depth_pp$depth == 2.1, ]
surface_pp$depth <- 0

depth_pp <- bind_rows(surface_pp, depth_pp)

## Have a look to the depth integrated PP
depth_pp %>%
  ggplot(aes(x = sum_day)) +
  geom_histogram(bins = 100) +
  facet_wrap(~ depth, scales = "free")

#' ### Daily integrated PP

## Calculate the integrated daily PP
daily_pp <- depth_pp %>%
  group_by(simul) %>%
  nest() %>%
  mutate(daily_pp = map(data, ~ pracma::trapz(.$depth, .$sum_day))) %>%
  unnest(daily_pp)

daily_pp %>%
  ggplot(aes(x = daily_pp)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = int_pp, col = "red", lty = 2)


#' ### Final error estimate

error <- sd(daily_pp$daily_pp)
error

#' Hence, the value of `r int_pp` +- `r error` should be reported in the paper.

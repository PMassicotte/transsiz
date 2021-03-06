

```r
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Calculate PP based on P vs I curves.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
```

Last updated: 2017-01-19 10:46:45
## Open the PS file and do some cleaning


```r
ps <- readxl::read_excel("data/Global_PS_for_Takuvik.xlsx") %>% 
  janitor::clean_names(case = "old_janitor") %>% 
  mutate(time = anytime::anytime(paste(date, format(time, "%H:%M:%S")))) %>% 
  mutate(time_numeric = as.numeric(time)) %>% 
  mutate(hour = as.numeric(format(time, "%H"))) %>% 
  mutate(par_just_below_ice_scalar_µmolquanta_corrected = par_just_below_ice_scalar_µmolquanta)
```

There is a problem with data later than 2015-06-20 20:20:00. Replace these "outliers" with the observations measured at the begining.


```r
i <- which(ps$time >= "2015-06-20 20:20:00")

ps$par_just_below_ice_scalar_µmolquanta_corrected[i] <- 
  ps$par_just_below_ice_scalar_µmolquanta_corrected[length(i):1]
```

## Plot the "raw" data
This is the raw data to work with.


```r
ps %>% 
  ggplot(aes(x = time, y = par_just_below_ice_scalar_µmolquanta)) +
  geom_point() +
  geom_point(aes(y = par_just_below_ice_scalar_µmolquanta_corrected), col = "red")
```

![plot of chunk unnamed-chunk-4](pp//unnamed-chunk-4-1.png)

## Hourly PAR


```r
res <- ps %>% 
  group_by(hour) %>% 
  nest() %>% 
  mutate(e = map(data, ~mean(.$par_just_below_ice_scalar_µmolquanta_corrected))) %>% 
  unnest(e)

res %>% 
  ggplot(aes(x = hour, y = e)) +
  geom_line() +
  geom_point()
```

![plot of chunk unnamed-chunk-5](pp//unnamed-chunk-5-1.png)

## Calculate e at each depth
This is done based on the integrated surface PAR and a kd value of 0.15 $m^{-1}$.


```r
compute_ez <- function(e) {

  kd <- -0.15 #m-1

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
```

![plot of chunk unnamed-chunk-6](pp//unnamed-chunk-6-1.png)

## PI curve
In this section we start by calculating photosynthetic parameters of one P vs E curve.


```r
# Open de PE data
df <- read_csv("data/pe.csv") %>% 
  fill(depth)
```

```
## Parsed with column specification:
## cols(
##   depth = col_double(),
##   light = col_double(),
##   p_manip = col_double()
## )
```

```r
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
  facet_wrap(~depth, scales = "free")
```

![plot of chunk unnamed-chunk-7](pp//unnamed-chunk-7-1.png)

```r
params <- mod %>% 
  unnest(coef) %>% 
  dplyr::select(depth, term, estimate) %>% 
  spread(term, estimate)

# Duplicate the first row and assume the same value at depth = 0 m

params <- rbind(params[1, ], params)
params$depth[1] <- 0

params
```

```
## # A tibble: 7 × 5
##   depth      alpha         beta          p0        ps
## * <dbl>      <dbl>        <dbl>       <dbl>     <dbl>
## 1   0.0 0.08011632 9.423394e-04 -0.01059057 3.2274108
## 2   2.1 0.08011632 9.423394e-04 -0.01059057 3.2274108
## 3   5.0 0.05447885 1.289265e-03  0.06624422 3.3129174
## 4  10.0 0.05437787 4.394171e-04  0.06799501 2.7518885
## 5  15.0 0.04359118 3.014396e-04  0.04435857 1.6443554
## 6  30.0 0.04008135 4.939990e-04  0.04474596 1.3912690
## 7  50.0 0.00462599 4.136213e-05  0.08170308 0.0957417
```

## Calculate hourly PP at each depth


```r
dat <- inner_join(insitu_light, params, by = "depth") %>% 
  mutate(p = ps * (1 - exp(-alpha * e_z / ps)) * exp(-beta * e_z / ps))

dat %>%
  ggplot(aes(x = hour, y = p)) +
  geom_point() +
  facet_wrap(~depth) +
  xlab("Hour of the day") + 
  ylab("PP")
```

![plot of chunk unnamed-chunk-8](pp//unnamed-chunk-8-1.png)

## Calculate integrated PP at each depth
Calculating the sum of PP at each depth gives us the daily PP at each particular depth.


```r
res <- dat %>%
  group_by(depth) %>% 
  summarise(sum_day = sum(p))

res %>% 
  ggplot(aes(x = sum_day, y = depth)) +
  geom_point() +
  geom_line() +
  scale_y_reverse()
```

![plot of chunk unnamed-chunk-9](pp//unnamed-chunk-9-1.png)

## Calculate the integrated PP
This is the integrated value of PP for 1 day. This is done by calculating the area under the curve of the daily PP at each depth.


```r
int_pp <- pracma::trapz(res$depth, res$sum_day)
int_pp
```

```
## [1] 240.5816
```

## Simulations
For each P vs E curve, I generate *n* simulated curves based a multivariate normal distribution of the fitted parameters. This will be used to estimate the error on the PP estimation.


```r
mod <- mod %>% 
  mutate(simulation = map2(model, data, simul, n = 10000)) # 10 simulations per depth

p <- map2(mod$simulation, mod$depth, plot_simulations)

cowplot::plot_grid(plotlist = p)
```

![plot of chunk unnamed-chunk-11](pp//unnamed-chunk-11-1.png)

At this point we have generated 10 000 P vs E curves. Let's have a look to the histogram for each photosynthetic parameter.


```r
plot_histo(mod)
```

![plot of chunk unnamed-chunk-12](pp//unnamed-chunk-12-1.png)

This is rassuring to see that they all follow a normal distribution. Now, we are ready to calculate the depth integrated PP.
### Depth integrated PP


```r
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
  facet_wrap(~depth, scales = "free")
```

![plot of chunk unnamed-chunk-13](pp//unnamed-chunk-13-1.png)

### Daily integrated PP


```r
## Calculate the integrated daily PP
daily_pp <- depth_pp %>% 
  group_by(simul) %>% 
  nest() %>% 
  mutate(daily_pp = map(data, ~pracma::trapz(.$depth, .$sum_day))) %>% 
  unnest(daily_pp)

daily_pp %>% 
  ggplot(aes(x = daily_pp)) +
  geom_histogram(bins = 50) +
  geom_vline(xintercept = int_pp, col = "red", lty = 2)
```

![plot of chunk unnamed-chunk-14](pp//unnamed-chunk-14-1.png)

### Final error estimate


```r
error <- sd(daily_pp$daily_pp)
error
```

```
## [1] 20.91984
```

Hence, the value of 240.5816434 +- 20.9198415 should be reported in the paper.

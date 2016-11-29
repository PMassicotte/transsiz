library(tidyverse)
library(extrafont)

## Clear the workspace
rm(list = ls())
graphics.off()

## Set default ggplot2 font size and font family
loadfonts(quiet = TRUE)
theme_set(theme_bw(base_size = 12, base_family = "Open Sans"))

## Do the calculation and generate the report
ezknitr::ezspin("R/pp.R", out_dir = "reports/", keep_html = FALSE, chunk_opts = list(tidy = TRUE))

## Publish it on github
system("sh ./publish.sh")

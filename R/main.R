# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>  
# AUTHOR:       Philippe Massicotte
#
# DESCRIPTION:  Main file for the Transsiz project.
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

# Setup -------------------------------------------------------------------

library(MASS)
library(tidyverse)
library(feather)
library(extrafont)

## Clear the workspace
rm(list = ls())
graphics.off()

## Set default ggplot2 font size and font family
loadfonts(quiet = TRUE)
theme_set(theme_bw(base_size = 12, base_family = "Open Sans"))

# Prepare data ------------------------------------------------------------

source("R/transmittance_christian.R")
source("R/pyrano_christian.R")
source("R/kd_christian.R")

# source("R/simulate_pe.R")
# 
# ## Do the calculation and generate the report
# ezknitr::ezspin("R/pp.R", out_dir = "reports/", keep_html = FALSE, chunk_opts = list(tidy = FALSE))
# 
# ## Publish it on github
# system("sh ./publish.sh")

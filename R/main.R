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
library(data.table)
library(sf)

## Clear the workspace
rm(list = ls())
graphics.off()

## Set default ggplot2 font size and font family
loadfonts(quiet = TRUE)
theme_set(theme_bw(base_size = 12, base_family = "IBM Plex Sans"))

# Prepare data ------------------------------------------------------------

## Ice station information
source("R/process_stations.R")

## Pyranometer
source("R/process_pyrano.R")

## ROV
source("R/process_rov.R")
source("R/rov_par_kd.R")

# source("R/pe_curves.R")
# source("R/transmittance_christian.R")
# source("R/kd_christian.R")
# source("R/pyrano_christian.R")
# 
# source("R/primary_production.R")
# source("R/process_cops.R")

## Embed fonts
files <- list.files("graphs/", full.names = TRUE)
lapply(files, embed_fonts)

## Create the report
rmarkdown::render("reports/methods.Rmd")

# source("R/simulate_pe.R")
# 
# ## Do the calculation and generate the report
# ezknitr::ezspin("R/pp.R", out_dir = "reports/", keep_html = FALSE, chunk_opts = list(tidy = FALSE))
# 
# ## Publish it on github
# system("sh ./publish.sh")

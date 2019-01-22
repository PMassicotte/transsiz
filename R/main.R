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
library(parallel)
library(multidplyr)
library(pbmcapply)
library(readxl)
library(furrr)
library(xtable)

## Clear the workspace
rm(list = ls())
graphics.off()

## Set default ggplot2 font size and font family
loadfonts(quiet = TRUE)
theme_set(theme_bw(base_size = 12, base_family = "IBM Plex Sans"))

plan(multiprocess)

# Prepare data ------------------------------------------------------------

## Ice station information
source("R/process_stations.R")

## Pyranometer
source("R/pyranometer/process_pyrano.R")

## ROV
source("R/rov/process_rov.R")
source("R/rov/rov_par_kd.R")
source("R/rov/rov_propagate_light_water_column.R")

## SUIT
source("R/suit/process_suit.R")
source("R/suit/suit_propagate_light_water_column.R")

## P vs E curves
source("R/pvse_curves.R")
source("R/pvse_propagate_parameters.R")

# Figures -----------------------------------------------------------------

source("R/fig1.R")
source("R/fig2.R")
source("R/fig3.R")
source("R/fig4.R")
source("R/fig5.R")
source("R/fig6.R")
source("R/fig7.R")

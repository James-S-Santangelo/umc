# Main script to run project scripts in correct order
#
# Author: James S. Santangelo

###############
#### SETUP ####
###############

# Load required packages
library(tidyverse)
library(broom)
library(car)
library(data.table)
library(ggeffects)
library(emmeans)
library(patchwork)
library(MetBrewer)
library(DHARMa)
library(lme4)
library(cowplot)
library(ggmap)
library(ggsn)
source("scripts/functions.R")

# Create all necessary output paths
paths <- c()
purrr::walk(paths, dir.create, show.warnings = FALSE)

################################
#### STEP 1: CLEAN RAW DATA ####
################################

# Clean up raw phenotypoe data for 5 parks
# Add distance of plant to park centre
# Add vegetation cover around plants
source('scripts/data-processing/create_allPlants_allParks.R')

##############################################################
#### STEP 2: CREATE SUMMARIES OF IBUTTON TEMPERATURE DATA ####
##############################################################

# Clean Ibutton data prior to summarising
source('scripts/data-processing/clean_iButton_csvs.R')

# Summarize data
source('scripts/data-processing/create_iButton_summaries.R')

##############################
#### STEP 3: RUN ANALYSES ####
##############################

# Run Analysis script. All analysis in same script since they're relatively short
source('scripts/analysisScript.R')

########################################################
#### STEP 4: FIGURES, TABLES, AND DESCRIPTIVE STATS ####
########################################################

# Figures and tables
source('scripts/tables_figures.R')

# Descriptive stats
source('scripts/descriptive_stats.R')

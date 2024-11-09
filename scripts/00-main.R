####################
# INSTALL PACKAGES #
####################

## from cran
from.cran <- c("cowplot", "data.table", "DemoDecomp", "ggh4x", 
               "ggnewscale", "furrr", "gt", "here", 
               "plyr", "tidyverse", "viridis")

## check if installed, else install
for(i in c(from.cran)){
  
  if(i %in% from.cran){
    
    if(system.file(package = i) == ""){install.packages(i)}
    
  }
}

#################
# LOAD PACKAGES #
#################

library(data.table)
library(furrr)
library(gt)
library(tidyverse)

############
# SET PATH #
############

here::i_am("scripts/00-main.R")

##################
# CREATE FOLDERS #
##################

if(!dir.exists(here::here("output"))){dir.create(here::here("output"))}
if(!dir.exists(here::here("output", "data"))){dir.create(here::here("output", "data"))}
if(!dir.exists(here::here("output", "figures"))){dir.create(here::here("output", "figures"))}
if(!dir.exists(here::here("output", "tables"))){dir.create(here::here("output", "tables"))}
if(!dir.exists(here::here("output", "csv"))){dir.create(here::here("output", "csv"))}

###############
# RUN SCRIPTS #
###############

source(here::here("scripts", "10-prep.R"), echo = TRUE, local = TRUE)
source(here::here("scripts", "09-functions.R"), echo = TRUE, local = TRUE)
source(here::here("scripts", "01-decomp.R"), echo = TRUE, local = TRUE)
source(here::here("scripts", "02-plots.R"), echo = TRUE, local = TRUE)
source(here::here("scripts", "03-tables.R"), echo = TRUE, local = TRUE)

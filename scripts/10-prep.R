############
# SET PATH #
############

here::i_am("scripts/10-prep.R")

###############
# SET OPTIONS #
###############

options(timeout = 10000)

#############
# LOAD DATA #
#############

## WHO
raw <-
  readRDS(file = here::here("data", "harmonized_all.rds"))[["y5"]] %>% 
  filter(source_mx == "type1",
         name != "Singapore",
         name != "Luxembourg") %>% 
  drop_na() %>%   
  rename(age = age_start) %>% 
  ## cause-specific mortality rate = cause-of-death share times all-cause mortality rate
  mutate(across(cause1:cause12, ~ .x * mx),
         name = case_when(name == "Czech Republic" ~ "Czechia",
                          TRUE ~ name)) %>% 
  select(name, year, sex, age, mx, cause1:cause12) %>% 
  arrange(name, year, sex, age)

## UNWPP
wpp <-
  data.table::fread(file = "https://population.un.org/wpp/Download/Files/1_Indicator%20(Standard)/CSV_FILES/WPP2024_Life_Table_Abridged_Medium_1950-2023.csv.gz") %>% 
  ## restrict to period 2015-2022
  filter(Time %in% 2015:2022)

#############
# SAVE DATA #
#############

## get downloaded date
date <- gsub("-", "", Sys.Date())

saveRDS(raw, here::here("data", "cod-raw.RDS"))
saveRDS(wpp, here::here("data", paste0("wpp2024_d", date, ".RDS")))

## clear environment
rm(list = ls())

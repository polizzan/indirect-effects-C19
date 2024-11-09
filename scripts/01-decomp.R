############
# SET PATH #
############

here::i_am("scripts/01-decomp.R")

#################
# LOAD WHO DATA #
#################

raw <-
  readRDS(here::here("data", "cod-raw.RDS"))

###############
# LIFE TABLES #
###############

## construct life tables by country, sex, and year
lt <-
  raw %>% 
  group_by(name, year, sex) %>% 
  group_modify(~ {
    
    life.table(df = .x)
    
  }) %>%
  ungroup()

#################
# DECOMPOSITION #
#################

data.decomp <- 
  raw %>%  
  ## remove all-cause mortality rates
  select(-mx) %>% 
  ## reshape data set to long format
  pivot_longer(cols = cause1:cause12, 
               names_to = "cause",
               values_to = "mx_cause") %>% 
  mutate(cause = factor(cause, levels = paste0("cause", 1:12))) %>% 
  arrange(name, sex, year, cause, age) %>% 
  split(f = list(.$name, .$sex))
  
## decomposition of life expectancy changes over time (by age and cause of death) 
## for each country-sex combination
plan(multisession, workers = 6) ## set number of workers for parallelization

decomposition.time <- 
  future_map(data.decomp, decomp.time, .progress = TRUE) %>% 
  bind_rows()

###############
# SAVE OUTPUT #
###############

saveRDS(lt, file = here::here("output", "data", paste0("e0.rds")))
saveRDS(decomposition.time, file = here::here("output", "data", paste0("decomposition_time.rds")))

## clear environment
rm(list = ls())
############
# SET PATH #
############

here::i_am("scripts/03-tables.R")

#############
# LOAD DATA #
#############

## load and modify WHO data
lt <- 
  readRDS(here::here("output", "data", "e0.rds"))

who <-
  lt %>% 
  filter(age == 0,
         year %in% c(2015, 2019, 2020, 2021, 2022)) %>% 
  select(name, year, sex, ex) %>%
  mutate(delta = c(0, diff(ex)), .by = c(name, sex))

## load and modify UNWPP data
wpp <- 
  readRDS(here::here("data", "wpp2024_d20240728.rds")) %>% 
  mutate(name = case_when(Location == "Republic of Korea" ~ "South Korea",
                          Location == "Russian Federation" ~ "Russia",
                          Location == "United States of America" ~ "USA",
                          TRUE ~ Location)) %>% 
  filter(name %in% unique(who$name), 
         AgeGrp == "0",
         Time %in% c(2015, 2019, 2020, 2021, 2022),
         Sex != "Total") %>%
  select(name, year = Time, sex = Sex, ex.wpp = ex) %>% 
  mutate(delta.wpp = c(0, diff(ex.wpp)), .by = c(name, sex))

## prepare data
e0 <-
  who %>% 
  left_join(wpp, by = c("name", "year", "sex")) %>% 
  mutate(indicator = case_when(delta > 0 & delta.wpp < 0 ~ "*",
                               delta < 0 & delta.wpp > 0 ~ "**",
                               TRUE ~ "")) %>% 
  mutate(delta = ifelse(delta < 0, paste0("-", sprintf("%03.1f", abs(delta))), paste0("+", sprintf("%03.1f", delta))),
         delta = paste0(delta, indicator)) %>% 
  select(sex, name, year, ex, delta) %>% 
  pivot_wider(names_from = c("year"),
              values_from = c("ex", "delta")) %>% 
  select(-delta_2015) %>% 
  arrange(sex, name)

############################
# TABULATE LIFE EXPECTANCY #
############################

for(x in c("Female", "Male")){
  
e0 %>% 
  filter(sex == x) %>% 
  mutate(sex = ifelse(duplicated(sex), "", sex)) %>%
  gt() %>% 
  tab_spanner(label = "Sex",
              columns = 1) %>% 
  tab_spanner(label = "Country",
              columns = 2) %>% 
  tab_spanner(label = "Life expectancy at birth",
              columns = 3:7) %>% 
  tab_spanner(label = "Changes in life expectancy",
              columns = 8:11) %>% 
  cols_label(sex = "",
             name = "",
             ex_2015 = "2015",
             ex_2019 = "2019",
             ex_2020 = "2020",
             ex_2021 = "2021",
             ex_2022 = "2022",
             delta_2019 = html("2015\U2013<br>2019"),
             delta_2020 = html("2019\U2013<br>2020"),
             delta_2021 = html("2020\U2013<br>2021"),
             delta_2022 = html("2021\U2013<br>2022")) %>% 
  tab_footnote(placement = "left",
               footnote = html("* Life expectancy increase in authors' dataset, but decrease in UNWPP.<br>
                               ** Life expectancy decrease in authors' dataset, but increase in UNWPP.")) %>% 
  fmt_number(columns = ex_2015:ex_2022,
             decimals = 1) %>% 
  tab_options(table_body.hlines.color = "white",
              table_body.vlines.color = "white",
              column_labels.border.bottom.width = px(0.5),
              column_labels.border.bottom.color = "black",
              row_group.border.top.style = "none",
              table_body.border.top.style = "none",
              column_labels.border.top.color = "white",
              table_body.border.bottom.style = "none",
              table.border.bottom.style = "none",
              table.font.size = px(12)) %>%  
  tab_style(
    style = cell_text(color = "black", weight = "bold"),
    locations = list(
      cells_column_spanners(everything()),
      cells_column_labels(everything())
    )) %>% 
  tab_style(style = cell_borders(
    sides = c("bottom"),  
    weight = px(0.5)),
    locations = cells_body(rows = c(length(unique(e0$name))))
    ) %>% 
  tab_style(style = cell_borders(
    sides = c("top"),  
    weight = px(0.5)),
    locations = cells_body(rows = c(1))
  ) %>% 
  sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = "\U2013"
  )  %>% 
  tab_options(table.font.names = "Times New Roman") %>% 
  cols_align(
    align = c("right"),
    columns = c("delta_2019", "delta_2020", "delta_2021", "delta_2022")
  ) %>% 
  gtsave(here::here("output", "tables", paste0("Table-1-", x, ".html"))) 
  
}  

####################
# SAVE AS CSV FILE #
####################

write.csv(e0 %>% 
            mutate(across(starts_with("delta"), ~ str_remove_all(., "\\*"))) %>% 
            mutate(across(-c(sex, name), ~ round(as.numeric(.), 1))),
          file = paste0(here::here("output", "csv"), "/life-expectancy.csv"),
          row.names = FALSE)

## clear environment
rm(list = ls())

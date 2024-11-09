############
# SET PATH #
############

here::i_am("scripts/02-plots.R")

##############################
# LOAD DECOMPOSITION RESULTS #
##############################

decomp_e0_time <- 
  readRDS(here::here("output", "data", "decomposition_time.rds"))

####################
# DEFINE CONSTANTS #
####################

## colors for plot
colors = viridis::turbo(n = 4)

## labels for causes of death in their current ordering
## long cause-of-death names will be displayed across two rows
labels_cause = c('Acute\nCVD', 'Other\nCVD', 'Acute\nrespiratory', 'COPD', 
                 'Infectious', 'Suicide', 'Drug', 'Alcohol', 
                 'Cancer', 'Other\nexternal', 'COVID-19', 'Residual') 

## new cause-of-death ordering
ordered_cause = c('COVID-19', 'Acute\nCVD', 'Other\nCVD', 'Cancer', 
                  'Acute\nrespiratory', 'COPD', 'Infectious', 
                  'Alcohol', 'Drug', 'Suicide', 'Other\nexternal', 'Residual') 

## separate the twelve causes of death into three groups
causes_fig_1 <- ordered_cause[1:4]
causes_fig_2 <- ordered_cause[5:8]
causes_fig_3 <- ordered_cause[9:12]

## country labels
levels_country = labels_country <- unique(decomp_e0_time$name)

## abbreviate country labels
labels_country[labels_country == "England and Wales"] <- "England & Wales"
labels_country[labels_country == "Northern Ireland"] <- "N. Ireland"

## age and year cut-offs for aggregation of decomposition results
time_cut = c(2015, 2019, 2020, 2021, 2022) 
age_cut = c(seq(0, 80, 10))

##################
# AGGREGATE DATA #
##################

## reorder and label variables 
decomp_e0_time[, sex := factor(x = sex, levels = c('Female', 'Male'))]

decomp_e0_time[, cause := factor(x = cause, labels = labels_cause)]
decomp_e0_time[, cause := factor(x = cause, levels = ordered_cause)]

decomp_e0_time[, name := factor(x = name, levels = levels_country, labels = labels_country)]

## cut and label year variable
decomp_e0_time[, period := cut(decomp_e0_time$year.final, 
                               breaks = c(time_cut[-length(time_cut)], Inf),
                               labels = paste0(time_cut[-length(time_cut)], '\U2013', time_cut[-1]))]

## reverse order year variable for plotting
decomp_e0_time[, period := factor(period, levels = rev(paste0(time_cut[-length(time_cut)], '\U2013', time_cut[-1])))]

## cut and label age variable
decomp_e0_time[, age_group := cut(decomp_e0_time$x + 1, 
                                  breaks = c(age_cut, Inf),
                                  labels = c(paste0(age_cut[-length(age_cut)], '\U2013', (age_cut[-1]-1)), paste0(age_cut[length(age_cut)], "+")))]

## aggregate over periods / age groups
decomp_e0_time_causes <- decomp_e0_time[, .(contribution = sum(contribution)), by = .(name, sex, period, cause)]
decomp_e0_time_causes_age <- decomp_e0_time[, .(contribution = sum(contribution)), by = .(name, sex, period, cause, age_group)]

##############
# MAIN PLOTS #
##############

## function to adjust plot labels
label.fun <- function(x){ifelse(x < 0, paste0("-", sprintf("%04.1f", abs(x))), sprintf("%04.1f", x))}

## function for plot
e0.plot <- function(s, df, limit.1, limit.2){
  
  ## order countries by COVID-19 contributions in 2019-2020
  cntry <-
    df %>% 
    filter(cause == "COVID-19",
           period == paste0("2019", "\U2013", "2020"),
           sex == s) %>% 
    arrange(contribution) %>% 
    pull(name)
  
  ## determine position of dots
  dot <- cntry[seq(1, length(cntry), 2)]
  
  ## prepare data set for plotting
  plot.df <-  
    df %>% 
    filter(sex == s)  %>%
    mutate(name = factor(name, levels = cntry),
           period = paste0(str_sub(period, 3, 4), "\U2013", str_sub(period, 8, 9)))
  
  ## plot
  ggplot() + 
    geom_tile(data = plot.df %>% filter(cause %in% c("COVID-19", "Non-\nCOVID-19")), 
              aes(x = period, y = name, fill = contribution)) + 
    labs(subtitle = paste("Contributions to changes in", tolower(s), "life expectancy"),
         x = NULL, , 
         y = NULL) + 
    scale_y_discrete(limits = rev) + 
    scale_fill_gradient2("Months", 
                         low = "#B2182B", mid = "#F7F7F7", high = "#2166AC", 
                         midpoint = 0, 
                         limits = c(-limit.1, limit.1),
                         breaks = c(-limit.1, 0, limit.1),
                         labels = label.fun,
                         guide = guide_colorbar(order = 1,
                                                title.position = "top")) +
    ggnewscale::new_scale_fill() + 
    geom_tile(data = plot.df %>% filter(!cause %in% c("COVID-19", "Non-\nCOVID-19")), 
              aes(x = period, y = name, fill = contribution)) +
    scale_fill_gradient2("Months",
                         low = "#C51B7D", mid = "#F7F7F7", high = "#4D9221", 
                         midpoint = 0, 
                         limits = c(-limit.2, limit.2),
                         breaks = c(-limit.2, 0, limit.2),
                         labels = label.fun,
                         guide = guide_colorbar(order = 2,
                                                title.position = "top")) +
    facet_wrap(~ cause, nrow = 1) + 
    geom_point(data = plot.df %>% 
                 filter(name %in% dot,
                        period %in% c(paste0("15", "\U2013", "19"),
                                      paste0("20", "\U2013", "21"))) %>% 
                 complete(nesting(name, cause), period), 
               aes(x = period, y = name), color = "black", size = 0.1, shape = 20) +
    theme_bw() + 
    theme(aspect.ratio = length(levels_country) / 4, 
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5),
          panel.background = element_rect(fill = "grey"), 
          panel.grid = element_blank(), 
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(size = 10), 
          axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
          axis.text.y = element_text(size = 10),
          axis.ticks.y = element_blank(),
          legend.title = element_text(size = 9, hjust = 0.5),
          legend.text = element_text(size = 9),
          legend.ticks = element_blank(),
          legend.key.height = unit(0.5, "cm"),
          legend.position = "bottom",
          panel.spacing.x = unit(c(0, 1.5, rep(0, length(unique(plot.df$cause)) - 3)), "lines"))
  
}

## data sets for plotting
## covid vs. non-covid
aux.1 <-
  decomp_e0_time_causes %>% 
  filter(!(cause == "COVID-19" & period == paste0("2015", "\U2013", "2019"))) %>% 
  ## annualize pre-pandemic contributions
  mutate(contribution = ifelse(period == paste0("2015", "\U2013", "2019"), 
                               contribution * 12 / 4, 
                               contribution * 12)) %>% 
  ## aggregate into covid and non-covid contributions
  mutate(cause = ifelse(cause == "COVID-19", "COVID-19", "Non-\nCOVID-19")) %>% 
  summarize(contribution = sum(contribution), .by = c(name, sex, period, cause))

## non-covid disaggregated
aux.2 <-
  decomp_e0_time_causes %>% 
  filter(!cause %in% c("COVID-19")) %>% 
  ## annualize pre-pandemic contributions
  mutate(contribution = ifelse(period == paste0("2015", "\U2013", "2019"), 
                               contribution * 12 / 4, 
                               contribution * 12)) %>% 
  summarize(contribution = sum(contribution), .by = c(name, sex, period, cause))

## non-covid aggregated
aux.3 <-
  aux.2 %>% 
  mutate(cause = case_when(str_detect(cause, "CVD") ~ "CVD",
                           cause %in% c("Alcohol", "Drug", 
                                        "Suicide", "Other\nexternal") ~ "Substance/\nexternal",
                           TRUE ~ cause)) %>% 
  summarize(contribution = sum(contribution), .by = c(name, sex, period, cause))

## find limits for gradient scale
lim.1 <-
  plyr::round_any(
    aux.1 %>% 
      pull(contribution) %>% 
      abs() %>% 
      max(), f = ceiling, accuracy = 0.1)

lim.2 <-
  plyr::round_any(
    aux.2 %>% 
      pull(contribution) %>% 
      abs() %>% 
      max(), f = ceiling, accuracy = 0.1)

lim.3 <-
  plyr::round_any(
    aux.3 %>% 
      pull(contribution) %>% 
      abs() %>% 
      max(), f = ceiling, accuracy = 0.1)

## combine data sets
df.1 <-
  aux.1 %>% 
  add_row(aux.2) %>% 
  mutate(cause = factor(cause, levels = c("COVID-19", "Non-\nCOVID-19", ordered_cause[-c(1)])))

df.2 <-
  aux.1 %>% 
  add_row(aux.3) %>%  
  mutate(cause = factor(cause, levels = c("COVID-19", "Non-\nCOVID-19", "CVD", ordered_cause[4:7], "Substance/\nexternal", ordered_cause[12])))

## plot
figSM <-
  map(c("Female", "Male"), e0.plot, df = df.1, limit.1 = lim.1, limit.2 = lim.2)

fig <-
  map(c("Female", "Male"), e0.plot, df = df.2, limit.1 = lim.1, limit.2 = lim.3)  

for(i in 1:2){
  
  g <- cowplot::get_plot_component(figSM[[i]], "guide-box", return_all = TRUE)
  
  figSM[[i]] <-
    cowplot::ggdraw(figSM[[i]] + 
                      labs(title = ifelse(i == 1, 
                                          paste0("Figure S5"), 
                                          paste0("Figure S6"))) +  
                      theme(plot.subtitle = element_text(size = 14, face = "plain", hjust = 0.5),
                            legend.position = "none", 
                            plot.margin = unit(c(0.2, 0.2, 1, 0.2), "inches"))) + 
    cowplot::draw_plot(g[[3]]$grobs[[1]], -0.3065, -0.8675) + 
    cowplot::draw_plot(g[[3]]$grobs[[2]], 0.121, -0.8675)
  
  g <- cowplot::get_plot_component(fig[[i]], "guide-box", return_all = TRUE)
  
  fig[[i]] <-
    cowplot::ggdraw(fig[[i]] + 
                      theme(legend.position = "none", 
                            plot.margin = unit(c(0.2, 0.2, 1, 0.2), "inches"))) + 
    cowplot::draw_plot(g[[3]]$grobs[[1]], -0.2625, -0.8675) + 
    cowplot::draw_plot(g[[3]]$grobs[[2]], 0.165, -0.8675)
  
}

## save plots
ggsave(here::here("output", "figures", paste0("Figure-1.pdf")),
       fig[[1]], width = 9, height = 7, device = pdf)

ggsave(here::here("output", "figures", paste0("Figure-2.pdf")),
       fig[[2]], width = 9, height = 7, device = pdf)

ggsave(here::here("output", "figures", paste0("Figure-S5-final.pdf")),
       figSM[[1]], width = 12, height = 7, device = pdf)

ggsave(here::here("output", "figures", paste0("Figure-S6-final.pdf")),
       figSM[[2]], width = 12, height = 7, device = pdf)

##################################
# SUPPLEMENTARY PLOTS: BAR PLOTS #
##################################

## set up matrix with country names, 3 countries per plot
res <- length(levels(decomp_e0_time_causes_age$name)) %% 3

if(res == 0){
  
  mat <- levels(decomp_e0_time_causes_age$name)
  
}else{
  
  mat <- c(levels(decomp_e0_time_causes_age$name), rep("", 3 - res))
  
}

mat <- 
  matrix(mat, nrow = 3)

## loop over sex, cause of death, & column of country matrix
plots.SM <-
  map(c("Female", "Male"), function(j){ 
    
    ## generate empty list for storing the plots
    plots <- list()
    
    ## z is used for storing the plots
    z <- 1
    
    for(k in 1:dim(mat)[2]){
      for(i in list(causes_fig_1, causes_fig_2, causes_fig_3)){
        
        ## prepare data set
        aux <- 
          decomp_e0_time_causes_age %>% 
          filter(cause %in% i) %>% 
          ## annualize pre-pandemic contributions
          mutate(contribution = ifelse(period == paste0("2015", "\U2013", "2019"), 
                                       contribution * 12 / 4, 
                                       contribution * 12))
        
        ## get limits for cause-specific panels
        min <- plyr::round_any(aux[contribution < 0] %>% 
                                 summarize(contribution = sum(contribution), 
                                           .by = c(name, cause, sex, age_group)) %>%
                                 summarize(contribution = min(contribution),
                                           .by = cause) %>% 
                                 arrange(cause) %>% 
                                 pull(contribution), 
                               f = floor, accuracy = 1)
        
        max <- plyr::round_any(aux[contribution > 0] %>% 
                                 summarize(contribution = sum(contribution), 
                                           .by = c(name, cause, sex, age_group)) %>%
                                 summarize(contribution = max(contribution),
                                           .by = cause) %>% 
                                 arrange(cause) %>% 
                                 pull(contribution), 
                               f = ceiling, accuracy = 1)
        
        ## plot
        fig.SM <- 
          ggplot(aux %>% filter(name %in% mat[,k] & sex == j), 
                 aes(x = age_group, y = contribution, fill = period)) + 
          geom_bar(stat = "identity", position = "stack", show.legend = TRUE) +
          geom_hline(yintercept = 0, linewidth = 0.25) +
          labs(x = NULL, 
               y = "Months",
               title = ifelse(j == "Female", 
                              paste0("Figure S7", letters[z]), 
                              paste0("Figure S8", letters[z])),
               subtitle = paste("Contributions to changes in", tolower(j), "life expectancy", "\n", "in",
                                paste(mat[,k][mat[,k] != ""], collapse = ", "))) +
          scale_fill_manual('', 
                            values = colors,
                            guide = guide_legend(reverse = TRUE)) +
          theme_bw() +
          theme(panel.grid = element_blank(),
                plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
                plot.subtitle = element_text(size = 14, hjust = 0.5),
                strip.background = element_rect(fill = "white"),
                strip.text = element_text(size = 14),
                axis.text.x = element_text(size = 10, angle = 315, hjust = 0),
                axis.text.y = element_text(size = 12),
                legend.position = "top",
                legend.justification = "center",
                legend.text = element_text(size = 12)) +
          ## display results for each country / cause-of-death category
          ggh4x::facet_nested(name ~ cause, 
                              scales = "free",
                              space = "free") +
          ## adjust labels in cause-of-death panels
          ggh4x::facetted_pos_scales(y = list(scale_y_continuous(limits = c(min[1], max[1]),
                                                                 breaks = c(min[1], min[1]/2, max[1]/2, max[1]),
                                                                 labels = label.fun),
                                              scale_y_continuous(limits = c(min[2], max[2]),
                                                                 breaks = c(min[2], min[2]/2, max[2]/2, max[2]),
                                                                 labels = label.fun),
                                              scale_y_continuous(limits = c(min[3], max[3]),
                                                                 breaks = c(min[3], min[3]/2, max[3]/2, max[3]),
                                                                 labels = label.fun),
                                              scale_y_continuous(limits = c(min[4], max[4]),
                                                                 breaks = c(min[4], min[4]/2, max[4]/2, max[4]),
                                                                 labels = label.fun))) +
          coord_flip()
        
        plots[[z]] <- fig.SM
        
        z <- z + 1  
        
      }
    }
    
    plots
    
  })

## save
cairo_pdf(filename = paste0(here::here("output", "figures"), "/Figure-S7-final.pdf"),
          width = 12,
          height = 9,
          onefile = TRUE)

plots.SM[[1]]

dev.off()

cairo_pdf(filename = paste0(here::here("output", "figures"), "/Figure-S8-final.pdf"),
          width = 12,
          height = 9,
          onefile = TRUE)

plots.SM[[2]]

dev.off()

####################
# SAVE AS CSV FILE #
####################

## main figures
decomp.main <-
  df.2 %>% 
  mutate(period = str_replace(period, "\U2013", "-"),
         cause = case_when(cause %in% c("Non-\nCOVID-19", "Substance/\nexternal") ~ str_replace(cause, "\n", ""),
                           TRUE ~ str_replace(cause, "\n", " ")),
         name = case_when(name == "England & Wales" ~ "England and Wales",
                          name == "N. Ireland" ~ "Northern Ireland",
                          TRUE ~ name)) %>% 
  arrange(name, sex, period, cause)

## supplementary figures
decomp.SM <-
  df.1 %>% 
  mutate(period = str_replace(period, "\U2013", "-"),
         cause = case_when(cause == "Non-\nCOVID-19" ~ str_replace(cause, "\n", ""),
                           TRUE  ~ str_replace(cause, "\n", " ")),
         name = case_when(name == "England & Wales" ~ "England and Wales",
                          name == "N. Ireland" ~ "Northern Ireland",
                          TRUE ~ name)) %>% 
  arrange(name, sex, period, cause)

## write to csv
write.csv(decomp.main,
          file = paste0(here::here("output", "csv"), "/decomp-main.csv"),
          row.names = FALSE)

write.csv(decomp.SM,
          file = paste0(here::here("output", "csv"), "/decomp-SM.csv"),
          row.names = FALSE)

## clear environment
rm(list = ls())
rm(list = ls())

## ----setup, include=FALSE, message=FALSE-------------------------------------------------------------------------------------------------------------------------------
library(pacman)

`%notin%` <- Negate(`%in%`)

p_load(
  #--- Packages to Fit Models
  MASS, logistf, survival, randcorr, JMI, 
  #--- Packages to Produce Tables
  gtsummary, flextable, janitor, broom, officer, kableExtra, reactable, 
  #--- Packages to Produce Figures
  crayon, ggsci, ggridges, ggthemes, ggforce, ggpubr, patchwork, grid, gridExtra, plotly,  survminer, viridis, ggridges, hrbrthemes, latex2exp, scales, glue, 
  #--- Packages for Data Retrieval & Pre-Processing
  readxl, here, rdrop2, lubridate, zoo, tidyverse, purrr, data.table, stringr, tidyr, vroom
)


#source("/home/soumikp/2022_jmva/code/functions.R")
#source("/home/soumikp/2022_jmva/code/simulation1/simulation1_patterns.R")

dir <- file.path("/home/soumikp/2022_jmva/output", "simulation4")
files <- list.files(dir, pattern = ".csv") 


mse <- function(estim, true){
  mean((estim - true)**2)
}

results <- NULL


results = matrix(file.path(dir, files) %>% map_dfr(., read.csv) %>% pull(2), byrow = T, ncol = 18)
colnames(results) = c("sampsize", "rho", "iter", 
                      "ex.emi", "ex.jmi", "ex.fmi", 
                      "ar1.emi", "ar1.jmi", "ar1.fmi", 
                      "spatial.emi", "spatial.jmi", "spatial.fmi", 
                      "mild.emi", "mild.jmi", "mild.fmi", 
                      "strong.emi", "strong.jmi", "strong.fmi")
                      
results <- as.tibble(results) %>% 
  group_by(sampsize, rho) %>%
  summarize(pow.ex.fmi = mean(ex.fmi <= 0.05),
            pow.ex.jmi = mean(ex.jmi <= 0.05),
            pow.ex.emi = mean(ex.emi <= 0.05),
            
            pow.ar1.fmi = mean(ar1.fmi <= 0.05),
            pow.ar1.jmi = mean(ar1.jmi <= 0.05),
            pow.ar1.emi = mean(ar1.emi <= 0.05),
            
            pow.spatial.fmi = mean(spatial.fmi <= 0.05),
            pow.spatial.jmi = mean(spatial.jmi <= 0.05),
            pow.spatial.emi = mean(spatial.emi <= 0.05),
            
            pow.mild.fmi = mean(mild.fmi <= 0.05),
            pow.mild.jmi = mean(mild.jmi <= 0.05),
            pow.mild.emi = mean(mild.emi <= 0.05),
            
            pow.strong.fmi = mean(strong.fmi <= 0.05),
            pow.strong.jmi = mean(strong.jmi <= 0.05),
            pow.strong.emi = mean(strong.emi <= 0.05),
            .groups = "drop") %>% 
  pivot_longer(cols = -c(sampsize, rho)) %>% 
  rowwise() %>% 
  mutate(family = str_to_title(unlist(strsplit(name, "[.]"))[2]), 
         type = unlist(strsplit(name, "[.]"))[3])
  
  

plot_title = "Comparison of empirical power of competing tests of independences based on mutual information (MI) for different configurations of various copula families for varying sample sizes."
plot_subtitle = "The dotted black line parallel to the x-axis denotes specified level of significance = 0.05."
plot_caption = "The estimation methods compared here are the empirical copula-based plugin MI (ecMI), the Fast Fourier transform-based MI (fastMI) and the jackknifed MI (JMI)."

p <- results %>% 
  mutate(name = factor(case_when(type == "fmi" ~ "fastMI", 
                                 type == "emi" ~ "ecMI", 
                                 type == "jmi" ~ "JMI"), 
                       levels = c("fastMI", "JMI", "ecMI"))) %>% 
  mutate(n = factor(paste0("n = ", sampsize), levels = c("n = 128", "n = 256"))) %>%  
  ggplot(aes(x = rho, y = value, color = name, linetype = name)) + 
  #geom_line(linewidth = 1) + 
  geom_hline(yintercept = 0.05, linewidth = 1, color = "black", linetype = "dotted") + 
  #geom_point() + 
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) + 
  facet_grid(rows = vars(family), 
             cols = vars(n), 
             scales = "free_x") + 
  xlab(TeX("$Kendall's \\tau$")) + 
  ylab("Empirical power") +
  scale_color_aaas() +
  theme_bw() + 
  theme(legend.position = "bottom", 
        
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, vjust = 0.5),
        axis.title = element_text(size = 12), 
        
        strip.text = element_text(size = 10, face = "bold", color = "white"), 
        strip.background = element_rect(fill = "black"),
        
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 10), 
        
        plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0, face = "italic"), 
        
       panel.spacing = unit(1.10,"lines")
  ) + 
  labs(color = "Estimation method", 
       linetype = "Estimation method", 
       title = str_wrap(plot_title, 95), 
       subtitle = str_wrap(plot_subtitle, 120),
       caption = str_wrap(plot_caption, 120)) + 
  scale_x_continuous(expand = c(0.0, 0.0)#,
                     #labels = scales::number_format(accuracy = 0.01, decimal.mark = '.')
                     ) 


ggsave(file.path("/home/soumikp/2022_jmva/tlfs", "simulation4_figure.pdf"), 
       p,
       width = 8.5,
       height = 11,
       units = "in",
       device = "pdf")


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

results <- read_csv(file.path(here(), "2023_jmva/output_raw", "simulation3.csv"))  

plot_title = "Comparison of empirical power of competing tests of independences based on mutual information (MI) for different configurations of various copula families for varying sample sizes."
plot_subtitle = "The dotted black line parallel to the x-axis denotes specified level of significance = 0.05."
plot_caption = "The estimation methods compared here are the empirical copula-based plugin MI (ECMI), the Fast Fourier transform-based MI (fastMI) and the jackknifed MI (JMI)."

p <- results %>% 
  mutate(name = factor(case_when(type == "emi" ~ "ECMI", 
                                 type == "fmi" ~ "fastMI", 
                                 type == "jmi" ~ "JMI"), 
                       levels = c("fastMI", "JMI", "ECMI"))) %>% 
  mutate(n = factor(paste0("n = ", sampsize), levels = c("n = 128", "n = 256"))) %>%  
  ggplot(aes(x = val_tau, y = value, color = name, linetype = name)) + 
  #geom_line(linewidth = 1) + 
  geom_hline(yintercept = 0.05, linewidth = 1, color = "black", linetype = "dotted") + 
  #geom_point() + 
  geom_smooth(method = "loess", se = FALSE, span = 0.4) + 
  facet_grid(rows = vars(family), 
             cols = vars(n)) + 
  xlab(TeX("$Kendall's \\tau$")) + 
  ylab("Empirical power") +
  scale_color_grey() +
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

ggsave(file.path(here(), "2023_jmva/output_processed", "simulation3_bw.pdf"),
       p,
       width = 8.5,
       height = 11,
       units = "in",
       device = "pdf")


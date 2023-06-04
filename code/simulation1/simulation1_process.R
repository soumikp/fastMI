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

results <- read_csv(file.path(here(), "simulation1.csv"))

plot_title = "Comparison of mean squared error (MSE) of mutual information (MI) estimation methods for different configurations of various copula families for varying sample sizes."
plot_subtitle = "The estimation methods compared here are the empirical copula-based plugin MI (ecMI), the Fast Fourier transform-based MI (fastMI) and the jackknifed MI (JMI)."

p <- results %>% 
  select(c(pat, n, tau, mse_fmi, mse_jmi, mse_emi)) %>%
  pivot_longer(cols = -c(pat, n, tau)) %>% 
  mutate(pat = case_when(pat == 1 ~  "Gaussian copula", 
                         pat== 2 ~ "Gumbel copula", 
                         pat== 3 ~ "Frank copula", 
                         pat== 4 ~ "Clayton copula")) %>% 
  mutate(name = factor(case_when(name == "mse_fmi" ~ "fastMI", 
                          name == "mse_emi" ~ "ecMI", 
                          name == "mse_jmi" ~ "JMI"), 
                       levels = c("fastMI", "JMI", "ecMI"))) %>% 
  mutate(n = factor(paste0("n = ", n), levels = c("n = 64", "n = 128", "n = 256"))) %>%  
  ggplot(aes(x = tau, y = value, color = name, linetype = name)) + 
  #geom_line(linewidth = 0.5) + 
  geom_smooth(method = "loess", se = FALSE) + 
  facet_grid(cols = vars(pat), 
             rows = vars(n)) + 
  xlab(TeX("Kendall's $\\tau$")) + 
  ylab("Mean squared error (MSE)") +
  scale_color_aaas() +
  theme_bw() + 
  theme(legend.position = "bottom", 
        
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12), 
        
        strip.text = element_text(size = 12, face = "bold", color = "white"), 
        strip.background = element_rect(fill = "black"),
        
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 10), 
        
        plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0, face = "italic")
  ) + 
  labs(color = "Estimation method", 
       linetype = "Estimation method", 
       title = str_wrap(plot_title, 95), 
       caption = str_wrap(plot_subtitle, 120)) + 
  scale_x_continuous(expand = c(0.0, 0.0)) 

ggsave(file.path(here(), "simulation1_figure.pdf"), 
       p,
       width = 8.5,
       height = 11,
       units = "in",
       device = "pdf")


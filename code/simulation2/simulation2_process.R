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

results <- read_csv(file.path(here(), "simulation2.csv"))

plot_title = "Comparison of mean squared error (MSE) of mutual information (MI) estimation methods for different correlation structures of the Gaussian copula for varying sample sizes in higher dimensions."
plot_subtitle = "The estimation methods compared here are the empirical copula-based plugin MI (ecMI), the Fast Fourier transform-based MI (fastMI) and the jackknifed MI (JMI)."

p <- results %>% 
  select(c(pat, n, rho, mse_fmi, mse_jmi, mse_emi)) %>%
  pivot_longer(cols = -c(pat, n, rho)) %>% 
  filter(!(pat > 3 & rho > 0.5)) %>% 
  mutate(pat = case_when(pat == 1 ~  "Exchangeable", 
                         pat== 2 ~ "AR(1)", 
                         pat== 3 ~ "Spatial", 
                         pat == 4 ~ "Block correlation: mild", 
                         pat == 5 ~ "Block correlation: strong")) %>% 
  mutate(name = factor(case_when(name == "mse_emi" ~ "ecMI", 
                          name == "mse_fmi" ~ "fastMI", 
                          name == "mse_jmi" ~ "JMI"), 
                       levels = c("fastMI", "JMI", "ecMI"))) %>% 
  mutate(n = factor(paste0("n = ", n), levels = c("n = 128", "n = 256", "n = 512"))) %>%  
  ggplot(aes(x = rho, y = value, color = name, linetype = name)) + 
  geom_smooth(method = "loess", se = FALSE) + 
  facet_grid(cols = vars(pat), 
             rows = vars(n), 
             scales = "free_x") + 
  xlab(TeX("$\\rho$")) + 
  ylab("Mean squared error (MSE)") +
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
       title = str_wrap(plot_title, 130), 
       caption = str_wrap(plot_subtitle, 160)) + 
  scale_x_continuous(expand = c(0.0, 0.0)#,
                     #labels = scales::number_format(accuracy = 0.01, decimal.mark = '.')
                     ) 

ggsave(file.path(here(), "simulation2_figure.pdf"), 
       p,
       height = 8.5,
       width = 11,
       units = "in",
       device = "pdf")


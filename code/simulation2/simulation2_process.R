rm(list = ls())

## ----setup, include=FALSE, message=FALSE-------------------------------------------------------------------------------------------------------------------------------
library(pacman)

`%notin%` <- Negate(`%in%`)

p_load(
  #--- Packages to Fit Models
  MASS, logistf, survival, randcorr, JMI, calibrate,
  #--- Packages to Produce Tables
  gtsummary, flextable, janitor, broom, officer, kableExtra, reactable, 
  #--- Packages to Produce Figures
  crayon, ggsci, ggridges, ggthemes, ggforce, ggpubr, patchwork, grid, gridExtra, plotly,  survminer, viridis, ggridges, hrbrthemes, latex2exp, scales, glue, 
  #--- Packages for Data Retrieval & Pre-Processing
  readxl, here, rdrop2, lubridate, zoo, tidyverse, purrr, data.table, stringr, tidyr, vroom
)

source(file.path(here(), "code", "patterns.R"))

mi_mvn <- function(Sigma, m_x, m_y){
  0.5*log((det(Sigma[m_x, m_x])*det(Sigma[m_y, m_y]))/det(Sigma))
}

scaleFUN <- function(x) sprintf("%.2f", x)

results <- read_csv(file.path(here(), "2023_jmva/output_raw", "simulation2.csv"))  

m_x = c(1,2)
m_y = c(3,4)

#### table ####
op <- results  %>%
  pivot_longer(cols = -c(pat, n, rho)) %>% 
  mutate(pat = case_when(pat== 1 ~  "CS", 
                         pat== 2 ~ "AR(1)", 
                         pat== 3 ~ "SP", 
                         pat== 4 ~ "Block:mild", 
                         pat==5 ~ "Block:strong")) %>% 
  mutate(name = factor(case_when(name == "fmi" ~ "fastMI", 
                                 name == "emi" ~ "ECMI", 
                                 name == "jmi" ~ "JMI"), 
                       levels = c("fastMI", "JMI", "ECMI"))) %>% 
  rowwise() %>% 
  mutate(mean = unlist(str_split(value, " "))[1], 
         sd = unlist(str_split(value, " "))[2], 
         mse = unlist(str_split(value, " "))[3],
         true = case_when(pat == "CS" ~ mi_mvn(Sigma_ex(rho, m_x, m_y), m_x, m_y),
                          pat == "AR(1)" ~ mi_mvn(Sigma_ar1(rho, m_x, m_y), m_x, m_y),
                          pat == "SP" ~ mi_mvn(Sigma_spatial(rho, m_x, m_y), m_x, m_y),
                          pat == "Block:mild" ~ mi_mvn(Sigma_between_mild(rho, m_x, m_y), m_x, m_y),
                          pat == "Block:strong" ~ mi_mvn(Sigma_between_strong(rho, m_x, m_y), m_x, m_y)), 
         pat = factor(case_when(pat == "CS" ~ "CS",
                                pat == "AR(1)" ~ "AR-1",
                                pat == "SP" ~ "Spatial",
                                pat == "Block:mild" ~ "Block (1/3)",
                                pat == "Block:strong" ~ "Block (2/3)"), 
                      levels = c("AR-1", "CS", "Spatial", 
                                 "Block (1/3)", 
                                 "Block (2/3)"))) %>% 
  mutate(mean = as.numeric(mean), 
         sd = as.numeric(sd), 
         mse = as.numeric(mse)) %>% 
  select(-value) %>% 
  mutate(n = factor(paste0("n = ", n), levels = c("n = 128", "n = 256", "n = 512"))) %>% 
  select(pat, n, rho, name, mse) %>% 
  filter(n == "n = 256") %>% 
  filter(rho <= 0.5) %>% 
  pivot_wider(values_from = mse, names_from = name) %>% 
  mutate(pct = paste0(round(100*(ECMI - fastMI)/ECMI), " (", round(100*(JMI - fastMI)/JMI), ")"))

op <- data.table(matrix(op$pct, byrow=T, ncol = 5, 
                  dimnames = list(sort(unique(op$rho)), 
                                  sort(unique(op$pat)))))

#xtable::xtable(op)

### plot ####

p_left <- results  %>%
  pivot_longer(cols = -c(pat, n, rho)) %>% 
  mutate(pat = case_when(pat== 1 ~  "CS", 
                         pat== 2 ~ "AR(1)", 
                         pat== 3 ~ "SP", 
                         pat== 4 ~ "Block:mild", 
                         pat==5 ~ "Block:strong")) %>% 
  mutate(name = factor(case_when(name == "fmi" ~ "fastMI", 
                          name == "emi" ~ "ECMI", 
                          name == "jmi" ~ "JMI"), 
                       levels = c("fastMI", "JMI", "ECMI"))) %>% 
  rowwise() %>% 
  mutate(mean = unlist(str_split(value, " "))[1], 
         sd = unlist(str_split(value, " "))[2], 
         mse = unlist(str_split(value, " "))[3],
         true = case_when(pat == "CS" ~ mi_mvn(Sigma_ex(rho, m_x, m_y), m_x, m_y),
                          pat == "AR(1)" ~ mi_mvn(Sigma_ar1(rho, m_x, m_y), m_x, m_y),
                          pat == "SP" ~ mi_mvn(Sigma_spatial(rho, m_x, m_y), m_x, m_y),
                          pat == "Block:mild" ~ mi_mvn(Sigma_between_mild(rho, m_x, m_y), m_x, m_y),
                          pat == "Block:strong" ~ mi_mvn(Sigma_between_strong(rho, m_x, m_y), m_x, m_y)), 
         pat = factor(case_when(pat == "CS" ~ "CS",
                         pat == "AR(1)" ~ "AR-1",
                         pat == "SP" ~ "Spatial",
                         pat == "Block:mild" ~ "Block (1/3)",
                         pat == "Block:strong" ~ "Block (2/3)"), 
                      levels = c("AR-1", "CS", "Spatial", 
                                 "Block (1/3)", 
                                 "Block (2/3)"))) %>% 
  mutate(mean = as.numeric(mean), 
         sd = as.numeric(sd), 
         mse = as.numeric(mse)) %>% 
  select(-value) %>% 
  mutate(n = factor(paste0("n = ", n), levels = c("n = 128", "n = 256", "n = 512"))) %>%  
  ggplot() + 
  geom_smooth(aes(x = rho, y = sqrt(mse), color = name, linetype = name), method = "loess", se = FALSE) + 
  facet_grid(cols = vars(pat), 
             rows = vars(n), 
             scales = "free_x") + 
  xlab(TeX("Pearson's  \\rho")) + 
  ylab("Residual mean squared error") +
  scale_color_grey() +
  scale_fill_grey() + 
  theme_bw() + 
  theme(legend.position = "bottom", 
        
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5),
        axis.title = element_text(size = 10), 
        
        strip.text = element_text(size = 10, color = "white"), 
        strip.background = element_rect(fill = "black"),
        
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8),
        
        panel.spacing = unit(0.75, "lines")
  ) + 
  labs(color = "Estimation method", 
       linetype = "Estimation method", 
       fill = "Estimation method"#, 
       #title = str_wrap(plot_title, 95), 
       #caption = str_wrap(plot_subtitle, 120)
       ) + 
  scale_x_continuous(expand = c(0.0, 0.0), labels=scaleFUN)


plot_title = "Comparison of residual mean squared error, absolute bias, and standard error of competing MI estimation methods for different configurations of some copula families for varying sample sizes."
plot_subtitle = "Comparisons of residual mean squared errors are presented in left panel, absolute biases in center panel and standard errors in right panel."
plot_caption = "The estimation methods compared here are the empirical copula-based plugin MI (ECMI), the Fast Fourier transform-based MI (fastMI) and the jackknifed MI (JMI)."


p <- (p_left) + 
  plot_annotation(title = str_wrap(plot_title, 135), 
                  caption = plot_caption#, 
                  #subtitle = plot_subtitle
                  ) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0, face = "italic")) 
 

ggsave(file.path(here(), "2023_jmva/output_processed", "simulation2_bw.pdf"),
       p,
       height = 8.5,
       width = 11,
       units = "in",
       device = "pdf")


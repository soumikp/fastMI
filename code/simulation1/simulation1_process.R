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

source(file.path(here(), "code", "patterns.R"))

scaleFUN <- function(x) sprintf("%.2f", x)

results <- read_csv(file.path(here(), "2023_jmva/output_raw/", "simulation1.csv"))  

#### tables ####
op <- results  %>%
  pivot_longer(cols = -c(pat, n, tau)) %>% 
  mutate(pat = case_when(pat == 1 ~  "Gaussian", 
                         pat== 2 ~ "Gumbel", 
                         pat== 3 ~ "Clayton")) %>% 
  mutate(name = factor(case_when(name == "fmi" ~ "fastMI", 
                                 name == "emi" ~ "ECMI", 
                                 name == "jmi" ~ "JMI"), 
                       levels = c("fastMI", "JMI", "ECMI"))) %>% 
  rowwise() %>% 
  mutate(mean = unlist(str_split(value, " "))[1], 
         sd = unlist(str_split(value, " "))[2], 
         mse = unlist(str_split(value, " "))[3],
         true = case_when(pat == "Gaussian" ~ tau_mi_gaussian(tau), 
                          pat == "Gumbel" ~ tau_mi_gumbel(tau), 
                          pat == "Clayton" ~ tau_mi_clayton(tau))) %>% 
  mutate(mean = as.numeric(mean), 
         sd = as.numeric(sd), 
         mse = as.numeric(mse)) %>% 
  select(-value) %>% 
  mutate(n = factor(paste0("n = ", n), levels = c("n = 64", "n = 128", "n = 256"))) %>% 
  select(pat, n, tau, name, mse) %>%
  group_by(pat, tau, n) %>% 
  pivot_wider(names_from = name, values_from = mse) %>%
  mutate(pct_emi = round(100*(ECMI - fastMI)/ECMI), 
         pct_jmi = round(100*(JMI - fastMI)/JMI)) %>% 
  mutate(pct1 = paste0(pct_emi, " (", pct_jmi, ")")) %>%
  ungroup() %>% 
  filter(n == "n = 256") #%>% 
  #group_by(pat, tau) %>% 
  #mutate(text0 = paste0(n,": ", pct1)) %>%
  #summarise(text1 = paste0(text0, collapse = "; "),
  #          .groups = "drop")

op <- data.frame(matrix(op$pct1, ncol = 3, nrow = 10,  byrow = T, 
                        dimnames = list(sort(unique(op$tau)), 
                                        sort(unique(op$pat)))))

#xtable::xtable(op)


### figures ####
p_left <- results  %>%
  pivot_longer(cols = -c(pat, n, tau)) %>% 
  mutate(pat = case_when(pat == 1 ~  "Gaussian", 
                         pat== 2 ~ "Gumbel", 
                         pat== 3 ~ "Clayton")) %>% 
  mutate(name = factor(case_when(name == "fmi" ~ "fastMI", 
                          name == "emi" ~ "ECMI", 
                          name == "jmi" ~ "JMI"), 
                       levels = c("fastMI", "JMI", "ECMI"))) %>% 
  rowwise() %>% 
  mutate(mean = unlist(str_split(value, " "))[1], 
         sd = unlist(str_split(value, " "))[2], 
         mse = unlist(str_split(value, " "))[3],
         true = case_when(pat == "Gaussian" ~ tau_mi_gaussian(tau), 
                          pat == "Gumbel" ~ tau_mi_gumbel(tau), 
                          pat == "Clayton" ~ tau_mi_clayton(tau))) %>% 
  mutate(mean = as.numeric(mean), 
         sd = as.numeric(sd), 
         mse = as.numeric(mse)) %>% 
  select(-value) %>% 
  mutate(n = factor(paste0("n = ", n), levels = c("n = 64", "n = 128", "n = 256"))) %>%  
  ggplot() + 
  geom_smooth(aes(x = tau, y = sqrt(mse), color = name, linetype = name), method = "loess", se = FALSE) + 
  facet_grid(cols = vars(pat), 
             rows = vars(n), 
             scales = "free_x") + 
  xlab(TeX("Kendall's  \\tau")) + 
  ylab("Residual mean squared error") +
  scale_color_aaas() +
  scale_fill_aaas() + 
  theme_bw() + 
  theme(legend.position = "bottom", 
        
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10), 
        
        strip.text.x = element_text(size = 10, color = "white"), 
        strip.background.x = element_rect(fill = "black"),
        
        strip.text.y = element_blank(), 
        strip.background.y = element_blank(),
        
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8)
  ) + 
  labs(color = "Estimation method", 
       linetype = "Estimation method", 
       fill = "Estimation method"#, 
       #title = str_wrap(plot_title, 95), 
       #caption = str_wrap(plot_subtitle, 120)
       ) + 
  scale_x_continuous(expand = c(0.0, 0.0), labels=scaleFUN)

p_center <- results  %>%
  pivot_longer(cols = -c(pat, n, tau)) %>% 
  mutate(pat = case_when(pat == 1 ~  "Gaussian", 
                         pat== 2 ~ "Gumbel", 
                         pat== 3 ~ "Clayton")) %>% 
  mutate(name = factor(case_when(name == "fmi" ~ "fastMI", 
                                 name == "emi" ~ "ECMI", 
                                 name == "jmi" ~ "JMI"), 
                       levels = c("fastMI", "JMI", "ECMI"))) %>% 
  rowwise() %>% 
  mutate(mean = unlist(str_split(value, " "))[1], 
         sd = unlist(str_split(value, " "))[2], 
         mse = unlist(str_split(value, " "))[3],
         true = case_when(pat == "Gaussian" ~ tau_mi_gaussian(tau), 
                          pat == "Gumbel" ~ tau_mi_gumbel(tau), 
                          pat == "Clayton" ~ tau_mi_clayton(tau))) %>% 
  mutate(mean = as.numeric(mean), 
         sd = as.numeric(sd), 
         mse = as.numeric(mse)) %>% 
  select(-value) %>% 
  mutate(n = factor(paste0("n = ", n), levels = c("n = 64", "n = 128", "n = 256"))) %>%  
  ggplot() + 
  geom_smooth(aes(x = tau, y = abs(true - mean), color = name, linetype = name), method = "loess", se = FALSE) + 
  facet_grid(cols = vars(pat), 
             rows = vars(n), 
             scales = "free_x") + 
  xlab(TeX("Kendall's  \\tau")) + 
  ylab("Absolute bias") +
  scale_color_aaas() +
  scale_fill_aaas() + 
  theme_bw() + 
  theme(legend.position = "bottom", 
        
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10), 
        
        strip.text.x = element_text(size = 10, color = "white"), 
        strip.background.x = element_rect(fill = "black"),
        
        strip.text.y = element_blank(), 
        strip.background.y = element_blank(),
        
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8)
  ) + 
  labs(color = "Estimation method", 
       linetype = "Estimation method", 
       fill = "Estimation method"#, 
       #title = str_wrap(plot_title, 95), 
       #caption = str_wrap(plot_subtitle, 120)
  ) + 
  scale_x_continuous(expand = c(0.0, 0.0), labels=scaleFUN)

p_right <- results  %>%
  pivot_longer(cols = -c(pat, n, tau)) %>% 
  mutate(pat = case_when(pat == 1 ~  "Gaussian", 
                         pat== 2 ~ "Gumbel", 
                         pat== 3 ~ "Clayton")) %>% 
  mutate(name = factor(case_when(name == "fmi" ~ "fastMI", 
                                 name == "emi" ~ "ECMI", 
                                 name == "jmi" ~ "JMI"), 
                       levels = c("fastMI", "JMI", "ECMI"))) %>% 
  rowwise() %>% 
  mutate(mean = unlist(str_split(value, " "))[1], 
         sd = unlist(str_split(value, " "))[2], 
         mse = unlist(str_split(value, " "))[3],
         true = case_when(pat == "Gaussian" ~ tau_mi_gaussian(tau), 
                          pat == "Gumbel" ~ tau_mi_gumbel(tau), 
                          pat == "Clayton" ~ tau_mi_clayton(tau))) %>% 
  mutate(mean = as.numeric(mean), 
         sd = as.numeric(sd), 
         mse = as.numeric(mse)) %>% 
  select(-value) %>% 
  mutate(n = factor(paste0("n = ", n), levels = c("n = 64", "n = 128", "n = 256"))) %>%  
  ggplot() + 
  geom_smooth(aes(x = tau, y = sd, color = name, linetype = name), method = "loess", se = FALSE) + 
  facet_grid(cols = vars(pat), 
             rows = vars(n), 
             scales = "free_x") + 
  xlab(TeX("Kendall's  \\tau")) + 
  ylab("Standard error") +
  scale_color_aaas() +
  scale_fill_aaas() + 
  theme_bw() + 
  theme(legend.position = "bottom", 
        
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10), 
        
        strip.text = element_text(size = 10, color = "white"), 
        strip.background = element_rect(fill = "black"),
        
        legend.title = element_text(size = 10, face = "bold"), 
        legend.text = element_text(size = 8)
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


p <- (p_left + p_center +  p_right) + 
  plot_annotation(title = str_wrap(plot_title, 135), 
                  caption = plot_caption, 
                  subtitle = plot_subtitle) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom',
        plot.title = element_text(size = 12, face = "bold"), 
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 10, hjust = 0, face = "italic")) 
 

ggsave(file.path(here(), "2023_jmva/output_processed/", "simulation1.pdf"), 
       p,
       height = 8.5,
       width = 11,
       units = "in",
       device = "pdf")


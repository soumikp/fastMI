rm(list = ls())
source(file.path(here(), "code/functions.R"))
source(file.path(here(), "code/patterns.R"))

require(tidyverse)
require(pracma)
require(MASS)

values = expand.grid(n = c(128, 256, 512), 
                     pattern = c(1, 2, 3, 4, 5), 
                     tau = sort(c((0:9/10))))

values = values %>% filter(!(pattern > 3 & tau > 0.5)) ## 126 parameter combinations

i = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")) ## cluster 
#i = sample(1:nrow(values), 1) ## running locally

n = values[i, 1]
pattern = values[i, 2]
rho = values[i, 3]

iter = 500

m_x = c(1,2) ## margin of X 
m_y = c(3,4) ## margin of Y

## exchangeable correlation
if(pattern == 1){
  target <- mi_mvn(Sigma_ex(rho, m_x, m_y), m_x, m_y)
  op <- zeros(n = iter, m = 3)
  for(i in 1:iter){
    data <- mvrnorm(n, mu = rep(0, (length(m_x) + length(m_y))), Sigma = Sigma_ex(rho, m_x, m_y))
    op[i, ] <- estim_mi(data, m_x, m_y)
  }
}

## ar1 correlation
if(pattern == 2){
  target <- mi_mvn(Sigma_ar1(rho, m_x, m_y), m_x, m_y)
  op <- zeros(n = iter, m = 3)
  for(i in 1:iter){
    data <- mvrnorm(n, mu = rep(0, (length(m_x) + length(m_y))), Sigma = Sigma_ar1(rho, m_x, m_y))
    op[i, ] <- estim_mi(data, m_x, m_y)
  }
}

## spatial correlation
if(pattern == 3){
  target <- mi_mvn(Sigma_spatial(rho, m_x, m_y), m_x, m_y)
  op <- zeros(n = iter, m = 3)
  for(i in 1:iter){
    data <- mvrnorm(n, mu = rep(0, (length(m_x) + length(m_y))), Sigma = Sigma_spatial(rho, m_x, m_y))
    op[i, ] <- estim_mi(data, m_x, m_y)
  }
}


## mild within, varying between
if(pattern == 4){
  target <- mi_mvn(Sigma_between_mild(rho, m_x, m_y), m_x, m_y)
  op <- zeros(n = iter, m = 3)
  for(i in 1:iter){
    data <- mvrnorm(n, mu = rep(0, (length(m_x) + length(m_y))), Sigma = Sigma_between_mild(rho, m_x, m_y))
    op[i, ] <- estim_mi(data, m_x, m_y)
  }
}

## strong within, varying between
if(pattern == 5){
  target <- mi_mvn(Sigma_between_strong(rho, m_x, m_y), m_x, m_y)
  op <- zeros(n = iter, m = 3)
  for(i in 1:iter){
    data <- mvrnorm(n, mu = rep(0, (length(m_x) + length(m_y))), Sigma = Sigma_between_strong(rho, m_x, m_y))
    op[i, ] <- estim_mi(data, m_x, m_y)
  }
}


op <- cbind(pattern, n, rho, target, op)
colnames(op) <- c("pat", "n", "rho", "true", "fmi", "jmi", "emi")

### not run - for saving ###
#output_folder = file.path("/home/soumikp/2022_jmva/output",  paste0("simulation2"),  paste0(pattern, "_", n, "_", rho, "_output.csv"))
#write.csv(op, output_folder)


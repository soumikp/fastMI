rm(list = ls())

require(tidyverse)
require(pracma)
require(MASS)

source("/home/soumikp/2022_jmva/code/patterns.R")
source("/home/soumikp/2022_jmva/code/functions.R")

r = 250

values = expand.grid(rho = (c((0:5/10))), 
                     n = c(128, 256), 
                     iter = c(1:100))

i = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
#i = sample(1:nrow(values), 1)

n = values[i,2]
rho = values[i,1]
iter = values[i,3]

m_x = c(1,2)
m_y = c(3,4)


## true data generated
true_ex <- mvrnorm(n, mu = rep(0, (length(m_x) + length(m_y))), Sigma = Sigma_ex(rho, m_x, m_y))
true_ar1 <- mvrnorm(n, mu = rep(0, (length(m_x) + length(m_y))), Sigma = Sigma_ar1(rho, m_x, m_y))
true_spatial <- mvrnorm(n, mu = rep(0, (length(m_x) + length(m_y))), Sigma = Sigma_spatial(rho, m_x, m_y))
true_mild <- mvrnorm(n, mu = rep(0, (length(m_x) + length(m_y))), Sigma = Sigma_between_mild(rho, m_x, m_y))
true_strong <- mvrnorm(n, mu = rep(0, (length(m_x) + length(m_y))), Sigma = Sigma_between_strong(rho, m_x, m_y))

## observed MI estimates calculated
obs_ex <- estim_mi(true_ex, m_x, m_y)
obs_ar1 <- estim_mi(true_ar1, m_x, m_y)
obs_spatial <- estim_mi(true_spatial, m_x, m_y)
obs_mild <- estim_mi(true_mild, m_x, m_y)
obs_strong <- estim_mi(true_strong, m_x, m_y)

## permuted MI estimates calculated
perm_ex <- replicate(r, estim_mi(cbind(true_ex[,m_x], true_ex[sample(nrow(true_ex), nrow(true_ex)), m_y]), m_x, m_y))
perm_ar1 <- replicate(r, estim_mi(cbind(true_ar1[,m_x], true_ar1[sample(nrow(true_ar1), nrow(true_ar1)), m_y]), m_x, m_y))
perm_spatial <- replicate(r, estim_mi(cbind(true_spatial[,m_x], true_spatial[sample(nrow(true_spatial), nrow(true_spatial)), m_y]), m_x, m_y))
perm_mild <- replicate(r, estim_mi(cbind(true_mild[,m_x], true_mild[sample(nrow(true_mild), nrow(true_mild)), m_y]), m_x, m_y))
perm_strong <- replicate(r, estim_mi(cbind(true_strong[,m_x], true_strong[sample(nrow(true_strong), nrow(true_strong)), m_y]), m_x, m_y))


## permutation-based pvalues calculated
pval_ex <- apply(rbind(t(perm_ex), obs_ex), 2, pval_helper)
pval_ar1 <- apply(rbind(t(perm_ar1), obs_ar1), 2, pval_helper)
pval_spatial <- apply(rbind(t(perm_spatial), obs_spatial), 2, pval_helper)
pval_mild <- apply(rbind(t(perm_mild), obs_mild), 2, pval_helper)
pval_strong <- apply(rbind(t(perm_strong), obs_strong), 2, pval_helper)

pvals <- c(n, rho, iter, 
           ex = pval_ex, ar1 = pval_ar1, spatial = pval_spatial, mild = pval_mild, strong = pval_strong)


## saving to folder
output_folder = file.path("/home/soumikp/2022_jmva/output", "simulation4",  
                          paste0(n, "_", rho, "_", iter, "_output.csv"))

write.csv(pvals, output_folder)



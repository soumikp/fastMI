rm(list = ls())
source("/home/soumikp/2022_jmva/code/functions.R")
source("/home/soumikp/2022_jmva/code/patterns.R")

r = 250
t = c(0:4)/10 + 0.05 
samp = c(128, 256)

values = expand.grid(t, samp, iter = c(1:165))

i = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
#i = sample(1:nrow(values), 1)

val_tau = values[i,1]
sampsize = values[i,2]
iter = values[i,3]

## true data generated
true_gauss = rgaussian(tau_theta_gaussian(val_tau), n = sampsize)
true_gumbel = rgumbel(tau_theta_gumbel(val_tau), n = sampsize)
true_clayton = rclayton(tau_theta_clayton(val_tau), n = sampsize)

## observed MI estimates calculated
obs_gauss <- estim_mi(true_gauss, 1, 2)
obs_gumbel <- estim_mi(true_gumbel, 1, 2)
obs_clayton <- estim_mi(true_clayton, 1, 2)

## permuted MI estimates calculated
perm_gauss <- replicate(r, estim_mi(cbind(true_gauss[,1], true_gauss[sample(nrow(true_gauss), nrow(true_gauss)),2]), 1, 2))
perm_gumbel <- replicate(r, estim_mi(cbind(true_gumbel[,1], true_gumbel[sample(nrow(true_gumbel), nrow(true_gumbel)),2]), 1, 2))
perm_clayton <- replicate(r, estim_mi(cbind(true_clayton[,1], true_clayton[sample(nrow(true_clayton), nrow(true_clayton)),2]), 1, 2))

## permutation-based pvalues calculated
pval_gauss <- apply(rbind(t(perm_gauss), obs_gauss), 2, pval_helper)
pval_gumbel <- apply(rbind(t(perm_gumbel), obs_gumbel), 2, pval_helper)
pval_clayton <- apply(rbind(t(perm_clayton), obs_clayton), 2, pval_helper)

pvals <- c(sampsize, val_tau, iter, gauss = pval_gauss, gumbel = pval_gumbel, clayton = pval_clayton)

## saving to folder
output_folder = file.path("/home/soumikp/2022_jmva/output", "simulation3",  
                          paste0(sampsize, "_", val_tau, "_", iter, "_output.csv")
                          )

write.csv(pvals, output_folder)

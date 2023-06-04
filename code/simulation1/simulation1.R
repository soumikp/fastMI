rm(list = ls())
source("/home/soumikp/2022_jmva/code/patterns.R")
source("/home/soumikp/2022_jmva/code/functions.R")

require(tidyverse)
require(pracma)

values = expand.grid(n = c(64, 128, 256), 
                     pattern = c(1, 2, 3), 
                     tau = sort(c((0:9/10))))

i = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
#i = sample(1:nrow(values), 1)

n = values[i, 1]
pattern = values[i, 2]
tau = values[i, 3]

iter = 250

## gaussian copula data
if(pattern == 1){
  target <- tau_mi_gaussian(tau)
  op <- zeros(n = iter, m = 3)
  for(i in 1:iter){
    data <- rgaussian(theta = tau_theta_gaussian(tau), n = n)
    op[i, ] <- estim_mi(data, 1, 2)
  }
}

## gumbel copula data
if(pattern == 2){
  target <- tau_mi_gumbel(tau)
  op <- zeros(n = iter, m = 3)
  for(i in 1:iter){
    data <- rgumbel(theta = tau_theta_gumbel(tau), n = n)
    op[i, ] <- estim_mi(data, 1, 2)
  }
}

## clayton copula
if(pattern == 3){
  target <- tau_mi_clayton(tau)
  op <- zeros(n = iter, m = 3)
  for(i in 1:iter){
    data <- rclayton(theta = tau_theta_clayton(tau), n = n)
    op[i, ] <- estim_mi(data, 1, 2)
  }
}

op <- cbind(pattern, n, tau, target, op)
colnames(op) <- c("pat", "n", "tau", "true", "fmi", "jmi", "emi")

output_folder = file.path("/home/soumikp/2022_jmva/output",  paste0("simulation1"),  paste0(pattern, "_", n, "_", tau, "_output.csv"))
write.csv(op, output_folder)


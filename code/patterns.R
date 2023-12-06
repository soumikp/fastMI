require(copula)
require(cubature)

#### For univariate margins (tables 1, 3; figures 1 and 3) ####

# given tau, calculate theta
tau_theta_gaussian <- function(tau){
  sin(pi*tau/2)
}
tau_theta_gumbel <- function(tau){
  1/(1-tau)
}
tau_theta_frank <- function(tau){
  
  optimiser_fun <- function(theta, tau){
    abs(1 - (4/theta)*(1 - debye1(theta)) - tau)
  }
  
  optimise(optimiser_fun, tau, interval=c(0.001, 100))$minimum
  
}
tau_theta_clayton <- function(tau){
  2*tau/(1 - tau)
}

# given tau, calculate true MI ##
tau_mi_gaussian <- function(t){
  tau <- sort(c(c(0:9)/10, 0.25, 0.75))
  true <- c(0, 0.012, 0.050, 0.079, 0.115, 0.212, 0.346, 0.531, 0.790, 0.960, 1.174, 1.855)
  true[tau == t]
}
tau_mi_gumbel <- function(t){
  tau <- sort(c(c(0:9)/10, 0.25, 0.75))
  true <- c(0,0.018, 0.064, 0.096, 0.136, 0.237, 0.375, 0.562, 0.821, 0.991, 1.205, 1.884)
  true[tau == t]
}
tau_mi_frank <- function(t){
  tau <- sort(c(c(0:9)/10, 0.25, 0.75))
  true <- c(0, 0.011, 0.046, 0.072, 0.105, 0.193, 0.316, 0.485, 0.723, 0.881, 1.082, 1.800)
  true[tau == t]
}
tau_mi_clayton <- function(t){
  tau <- sort(c(c(0:9)/10, 0.25, 0.75))
  true <- c(0, 0.019, 0.072, 0.111, 0.157, 0.275, 0.432, 0.636, 0.911, 1.089, 1.308, 2.003)
  true[tau == t]
}

# given theta, generate data from copula with std normal marginals
rgaussian <- function(theta, n=250){
  obj <- mvdc(normalCopula(theta), c("norm", "norm"), 
              list(list(mean=0, sd=1), list(mean=0, sd=1)))
  rMvdc(n, obj)
}
rgumbel <- function(theta, n=250){
  obj <- mvdc(gumbelCopula(theta), c("norm", "norm"), 
              list(list(mean=0, sd=1), list(mean=0, sd=1)))
  rMvdc(n, obj)
}
rfrank <- function(theta, n=250){
  obj <- mvdc(frankCopula(theta), c("norm", "norm"), 
              list(list(mean=0, sd=1), list(mean=0, sd=1)))
  rMvdc(n, obj)
}
rclayton <- function(theta, n=250){
  obj <- mvdc(claytonCopula(theta), c("norm", "norm"), 
              list(list(mean=0, sd=1), list(mean=0, sd=1)))
  rMvdc(n, obj)
}


#### multivariate marginals (simulations 2 and 4) ####

## dispersion matrix for exchangeable structure - one parameter
Sigma_ex <- function(rho_ex, m_x, m_y){
  return(((1 - rho_ex)*diag(length(m_x) + length(m_y)) + rho_ex*calibrate::ones(length(m_x) + length(m_y))))
}

## dispersion matrix for AR1 structure - one parameter
Sigma_ar1 <- function(rho_ar1, m_x, m_y){
  dim = length(m_x) + length(m_y)
  sigma <- diag(dim)
  for(row in 1:dim){
    for(col in 1:dim){
      sigma[row, col] = sigma[row, col] + ifelse(row == col, 0, rho_ar1^(abs(row-col)))
    }
  }
  return(sigma)
}

## dispersion matrix for spatial structure - one parameter
Sigma_spatial <- function(rho_spatial, m_x, m_y){
  dim = length(m_x) + length(m_y)
  sigma <- diag(dim)
  for(row in 1:dim){
    for(col in 1:dim){
      sigma[row, col] = sigma[row, col] + ifelse(row == col, 0, exp(-abs(row-col)/(10*rho_spatial)))
    }
  }
  return(sigma)
}



## dispersion for between group correlation with mild within group correlation
Sigma_between_mild <- function(rho_between, m_x, m_y){
  dim = length(m_x) + length(m_y)
  sigma <- diag(dim)
  sigma_xx <- (1 - 0.3)*diag(length(m_x)) + 0.3*calibrate::ones(length(m_x))
  sigma_yy <- (1 - 0.3)*diag(length(m_x)) + 0.3*calibrate::ones(length(m_x))
  sigma_xy <- rho_between*calibrate::ones(n = length(m_x), p = length(m_y))
  sigma = rbind(cbind(sigma_xx, sigma_xy), cbind(t(sigma_xy), sigma_yy))
  return(sigma)
}

## dispersion for between group correlation with strong within group correlation
Sigma_between_strong <- function(rho_between, m_x, m_y){
  dim = length(m_x) + length(m_y)
  sigma <- diag(dim)
  sigma_xx <- (1 - 0.6)*diag(length(m_x)) + 0.6*calibrate::ones(length(m_x))
  sigma_yy <- (1 - 0.6)*diag(length(m_x)) + 0.6*calibrate::ones(length(m_x))
  sigma_xy <- rho_between*calibrate::ones(n = length(m_x), p = length(m_y))
  sigma = rbind(cbind(sigma_xx, sigma_xy), cbind(t(sigma_xy), sigma_yy))
  return(sigma)
}


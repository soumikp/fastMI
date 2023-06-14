# fastMI: a fast and consistent copula-based nonparametric estimator of mutual information

> Purkayastha, S., & Song, P. X. (2022). fastMI: a fast and consistent copula-based estimator of mutual information. arXiv preprint arXiv:2212.10268.

In this brief tutorial we illustrate the usage of the functions written for this package.

## Preliminaries

We will need to install the following functions

```
require(reticulate)
require(reticulate)
require(ks)
require(copula)
require(here)
```

Having installed these functions, please run the following two commands to load the functions which will (i) estimate $MI$ through various methods: 
```
source(file.path(here(), "code/functions.R"))
```
and (ii) generate data for simulation studies:
```
source(file.path(here(), "code/patterns.R"))
```

## Estimating MI

We define a multivariate normal structure (please see the code block below) with AR-1 correlation pattern. We fix $\rho = 0.5$ and calculate the true $MI = 0.1438$. 

```
## generating AR-1 structure with rho = 0.5
true_sigma = Sigma_ar1(0.5, ## ar-1 with rho = 0.5
                       m_x = c(1, 2), ## margins of X
                       m_y = c(3, 4) ## margins of Y
                       )

data <- MASS::mvrnorm(n = 500, ## sample size = 500
                      mu = c(1, 1, 2, 2), ## bivariate X has mean (1, 1); bivariate Y has mean (2, 2)
                      Sigma = true_sigma)

true_mi <- mi_mvn(true_sigma, m_x = c(1, 2), m_y = c(3, 4))
```

The functions `estim_emi`, `estim_jmi` and `estim_fmi` calculate the ECMI, JMI and fastMI estimators respectively. Please see the example below for more details. The output for one such run is also provided below.

```
c("True MI" = true_mi, 
  "ECMI" = estim_emi(data, m_x = c(1, 2), m_y = c(3, 4)), 
  "JMI" = estim_jmi(data, m_x = c(1, 2), m_y = c(3, 4)), 
  "fastMI" = estim_fmi(data, m_x = c(1, 2), m_y = c(3, 4)))
```

```
  True MI      ECMI       JMI    fastMI 
0.1438410 0.8263204 0.3183082 0.1191138
```

## Permutation test for independence

Given two random vectors $\mathbf{X}$ and $\mathbf{Y}$ we can always permute one vector as follows to get permuted estimates of $MI$ which will lead to an empirical distribution of $MI$ under the null hypothesis of independence of  $\mathbf{X}$ and $\mathbf{Y}$.

```
obs_mi <- estim_fmi(data, m_x, m_y)
perm_mi <- replicate(r, estim_fmi(cbind(data[,m_x], data[sample(nrow(data), nrow(data)), m_y]), m_x, m_y))
pval_mi <- sum(perm_mi > obs_mi)/length(perm_mi)
```
Here `pval_mi` will return the permutation-based p-value for the test.





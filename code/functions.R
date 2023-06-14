require(reticulate)
py_install("fastkde", pip = TRUE)
#source_python(file.path(here(), "code/fastkde_mi.py"))

require(JMI) # for JMI
require(pracma)
require(ks)
require(copula) # for empirical copula density estimation

estim_emi <- function(data, m_x, m_y){
  
  n = nrow(data)
  x = copula::F.n(pobs(as.matrix(data[,m_x])), pobs(as.matrix(data[,m_x])))
  y = copula::F.n(pobs(as.matrix(data[,m_y])), pobs(as.matrix(data[,m_y])))
  
  xy = matrix(c(x,y), n, 2)
  H = Hpi.diag(xy)
  
  fx = kde(x, h=kde(x)$h, eval.point=x)$estimate
  fy = kde(y, h=kde(y)$h, eval.point=y)$estimate
  fxy = kde(xy, H=H, eval.point=xy)$estimate
  
  mi = mean(log(fxy/(fx*fy)))
  
  return((mi))
  
}

estim_jmi <- function(data, m_x, m_y){
  return(JMI::JMI(as.matrix(data[,m_x]), as.matrix(data[,m_y]), BN = 0)$mi)
}

estim_fmi <- function(data, m_x, m_y){
  return(fastkde_estim(copula::F.n(pobs(as.matrix(data[,m_x])), pobs(as.matrix(data[,m_x]))),
                       copula::F.n(pobs(as.matrix(data[,m_y])), pobs(as.matrix(data[,m_y])))))
}

estim_mi <- function(data, m_x, m_y){
  return(c(fmi = estim_fmi(data, m_x, m_y),
           jmi = estim_jmi(data, m_x, m_y),
           emi = estim_emi(data, m_x, m_y)))
}

mi_mvn <- function(Sigma, m_x, m_y){
  0.5*log((det(Sigma[m_x, m_x])*det(Sigma[m_y, m_y]))/det(Sigma))
}

pval_helper <- function(x){
  obs <- x[length(x)]
  perm <- x[-length(x)]
  return(sum(perm>obs)/length(perm))
}
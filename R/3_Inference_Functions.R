#Get asymptotic variance (\gamma^2) using plug in method (general)
get_asym_var_general<-function(x,y,z){
  #First, get the parameter matrix, 7*K
  #The 7 rows are: p_s,rho_s,mu_X4_s,mu_X3Y_s,mu_X2Y2_s,mu_XY3_s,mu_Y4_s
  #_s stands for specified
  n<-length(x)
  z_uniq<-unique(z)
  results<-sapply(z_uniq,FUN=function(z_k){
    idx<-which(z==z_k)
    p_k<-length(idx)/n #Returned
    x_k<-x[idx]
    y_k<-y[idx]
    rho_k<-cor(x_k,y_k) #Returned
    x_k_std<-(x_k-mean(x_k))/sd(x_k)
    y_k_std<-(y_k-mean(y_k))/sd(y_k)
    return(c(p_k,
             rho_k,
             mean(x_k_std^4),
             mean(x_k_std^3*y_k_std),
             mean(x_k_std^2*y_k_std^2),
             mean(x_k_std*y_k_std^3),
             muY4_k=mean(y_k_std^4)))
  }) #7*K

  #Second, store the rows of the parameter matrix as vectors
  p_s<-results[1,] #Vector of length K
  rho_s<-results[2,] #Vector of length K
  mu_X4_s<-results[3,] #Vector of length K
  mu_X3Y_s<-results[4,] #Vector of length K
  mu_X2Y2_s<-results[5,] #Vector of length K
  mu_XY3_s<-results[6,] #Vector of length K
  mu_Y4_s<-results[7,] #Vector of length K

  #Third, calculate asymptotic variance
  A_s<-p_s*
    (rho_s^4*(mu_X4_s+2*mu_X2Y2_s+mu_Y4_s)-4*rho_s^3*(mu_X3Y_s+mu_XY3_s)+4*rho_s^2*mu_X2Y2_s) #Vector of length K
  B_s<-p_s*(1-p_s)*rho_s^4 #Vector of length K
  C_s<-(-1)*outer(p_s*rho_s^2,p_s*rho_s^2) #K*K
  diag(C_s)<-0
  toReturn<-sum(A_s)+sum(B_s)+sum(C_s)
  return(toReturn)
}

#Get asymptotic variance (\gamma^2) using plug in method (bivariate normal)
get_asym_var_binorm<-function(x,y,z){
  #First, get the parameter matrix, 2*K
  #The 2 rows are: p_s,rho_2_s
  #_s stands for specified
  n<-length(x)
  z_uniq<-unique(z)
  results<-sapply(z_uniq,FUN=function(z_k){
    idx<-which(z==z_k)
    p_k<-length(idx)/n #Returned
    x_k<-x[idx]
    y_k<-y[idx]
    rho_2_k<-cor(x_k,y_k)^2 #Returned
    return(c(p_k,
             rho_2_k))
  }) #2*K

  #Second, store the rows of the parameter matrix as vectors
  p_s<-results[1,] #Vector of length K
  rho_2_s<-results[2,] #Vector of length K

  #Third, calculate asymptotic variance
  A_s<-4*p_s*rho_2_s*(1-rho_2_s)^2 #Vector of length K
  B_s<-p_s*(1-p_s)*rho_2_s^2 #Vector of length K
  C_s<-(-1)*outer(p_s*rho_2_s,p_s*rho_2_s) #K*K
  diag(C_s)<-0
  toReturn<-sum(A_s)+sum(B_s)+sum(C_s)
  return(toReturn)
}


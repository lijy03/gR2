#Unspecified scenario
#First,
#if K is chosen, get membership.
#If K is not chosen, choose K and get membership (and print explanations along the way if verbose).
#Then,
#if inference is false, then return a list of three item: estimate, K, membership.
#If inference is true, then return a list of six items: estimate, conf.level, conf.int, p.val, K, membership.
gR2_Unspecified<-function(x,y,
                          K,cand.Ks,num_init,mc.cores,regressionMethod,verbose,
                          inference,conf.level,gR2.pop,alternative,method){
  #num_init (number of initializations)
  #num_init is 30 by default. However, when n is smaller than 50, num_init is set to be larger.
  n<-length(x)
  if (n<50){
    num_init<-floor(1500/n)
  }

  #K chosen vs. not chosen
  if(!is.null(K)){
    #If K is chosen, get membership.
    result<-Klines(x,y,
                   K,num_init,mc.cores,regressionMethod)
    membership<-result$membership
    #W<-result$W #Don't need to store W when K is chosen
  }else{
    #If K is not chosen, choose K and get membership (and print explanations along the way if verbose).
    result<-gR2_Unspecified_Choose_K(x,y,
                                     cand.Ks,num_init,mc.cores,regressionMethod,verbose)
    K<-result$K
    membership<-result$membership
  }

  #Save output from gR2_Specified in a list, and then add K and membership to it.
  toReturn1<-gR2_Specified(x,y,z=membership,
                           inference,conf.level,gR2.pop,alternative,method)
  toReturn2<-list(K=K,membership=membership)
  toReturn<-c(toReturn1,toReturn2)

  #Return
  return(toReturn)
}

#Choose K
#Returns a list of two items: chosen K and membership; prints explanations along the way if verbose
gR2_Unspecified_Choose_K<-function(x,y,
                                   cand.Ks,num_init,mc.cores,regressionMethod,verbose){
  #If verbose, print "Candidate K values: 1, 2, 3, 4" (by default)
  if(verbose) cat("Candidate K values: ",paste(cand.Ks,collapse=", "),"\n",sep="")

  #Run Klines on each candidate K
  results<-lapply(cand.Ks,FUN=function(cand.K){
    return(Klines(x,y,
                  K=cand.K,num_init,mc.cores,regressionMethod))
  }) #A list of 4 lists (by default), each of length 2 (membership and W)

  #Get Ws
  Ws<-sapply(results,FUN=function(result){
    return(result$W)
  }) #Vector of length 4 (by default)

  #Get AICs
  AICs<-sapply(results,FUN=function(result){
    membership<-result$membership
    return(getAIC(x,y,membership))
  }) #Vector of length 4 (by default)

  #Choose K
  K<-cand.Ks[which.min(AICs)]

  #If verbose, print "The K value chosen by AIC is 2." (if 2 is chosen)
  if(verbose) cat("The K value chosen by AIC is ",K,".","\n",sep="")

  #If verbose, plot W and AIC against candidate K
  if(verbose){
    par(mfrow=c(2,1),mar=c(3,3,2,1)) #"mar" means margin
    #Top panel: Scree plot
    plot(cand.Ks,Ws,type="l",main="Scree plot")
    title(xlab="K",ylab="Avg. sq. perp./vert. dist.",line=2)
    abline(v=K,lty=2) #lyt means line type.
    #Bottom panel: Choose K by AIC
    plot(cand.Ks,AICs,type="l",main="Choose K by AIC")
    title(xlab="K",ylab="AIC",line=2)
    abline(v=K,lty=2) #lyt means line type.
  }

  membership<-results[[which.min(AICs)]]$membership
  toReturn<-list(K=K,membership=membership)
  return(toReturn)
}

getAIC<-function(x,y,membership){
  n<-length(x)
  cand.K<-length(unique(membership))
  joint_dens<-sapply(1:cand.K,FUN=function(k){
    idx<-which(membership==k)
    n_k<-length(idx)
    p_k_hat<-n_k/n

    x_k<-x[idx]
    y_k<-y[idx]
    mu_k_hat<-c(mean(x_k),mean(y_k))
    x_k_c<-x_k-mu_k_hat[1]
    y_k_c<-y_k-mu_k_hat[2]
    data_k_c<-cbind(x_k_c,y_k_c)
    Sigma_k_hat<-t(data_k_c)%*%data_k_c/n_k
    return(mvtnorm::dmvnorm(cbind(x,y),mean=mu_k_hat,sigma=Sigma_k_hat,log=FALSE)*p_k_hat)
  }) #nxK, the joint densities of (X_i,Y_i,Z_i)
  marginal_dens<-rowSums(joint_dens) #Vector of length n, the marginal densities of (X_i,Y_i)
  AIC<-2*(6*cand.K-1)-2*sum(log(marginal_dens))
  return(AIC)
}


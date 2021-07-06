#Unspecified scenario
#First,
#if K is chosen, get membership.
#If K is not chosen, choose K and get membership (and print explanations along the way if verbose).
#Then,
#if inference is false, then return a list of three item: estimate, K, membership.
#If inference is true, then return a list of six items: estimate, conf.level, conf.int, p.val, K, membership.
#Also, if details is true, then include perLineInfo in the list as well.
gR2_Unspecified<-function(x,y,
                          K,cand.Ks,num_init,mc.cores,regressionMethod,verbose,
                          inference,conf.level,gR2.pop,alternative,method,
                          details,
                          genotypeVector,minRelativeGroupSize,mod.sel){
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
                   K,num_init,mc.cores,regressionMethod,
                   genotypeVector,minRelativeGroupSize)
    membership<-result$membership
    #W<-result$W #Don't need to store W when K is chosen
  }else{
    #If K is not chosen, choose K and get membership (and print explanations along the way if verbose).
    result<-gR2_Unspecified_Choose_K(x,y,
                                     cand.Ks,num_init,mc.cores,regressionMethod,verbose,
                                     genotypeVector,minRelativeGroupSize,mod.sel)
    K<-result$K
    membership<-result$membership
  }

  #Save output from gR2_Specified in a list, and then add K, membership, and possibly perLineInfo to it.
  toReturn1<-gR2_Specified(x,y,z=membership,
                           inference,conf.level,gR2.pop,alternative,method)
  toReturn2<-list(K=K,membership=membership)
  if(details==FALSE){
    toReturn<-c(toReturn1,toReturn2)
  }else{
    perLineInfo<-getPerLineInfo(x,y,K,membership)
    toReturn3<-list(perLineInfo=perLineInfo)
    toReturn<-c(toReturn1,toReturn2,toReturn3)
  }

  #Return
  return(toReturn)
}

#Choose K
#Returns a list of two items: chosen K and membership; prints explanations along the way if verbose
gR2_Unspecified_Choose_K<-function(x,y,
                                   cand.Ks,num_init,mc.cores,regressionMethod,verbose,
                                   genotypeVector,minRelativeGroupSize,mod.sel){
  #If verbose, print "Candidate K values: 1, 2, 3, 4" (by default)
  if(verbose) cat("Candidate K values: ",paste(cand.Ks,collapse=", "),"\n",sep="")

  #Run Klines on each candidate K
  results<-lapply(cand.Ks,FUN=function(cand.K){
    return(Klines(x,y,
                  K=cand.K,num_init,mc.cores,regressionMethod,
                  genotypeVector,minRelativeGroupSize))
  }) #A list of 4 lists (by default), each of length 2 (membership and W)

  #Get Ws
  Ws<-sapply(results,FUN=function(result){
    return(result$W)
  }) #Vector of length 4 (by default)

  #Filter out candidate K's with W=Inf (which only happens when K is greater than 1)
  indicesOfCandKsToKeep<-which(Ws<Inf)
  cand.Ks<-cand.Ks[indicesOfCandKsToKeep]
  results<-results[indicesOfCandKsToKeep]
  Ws<-Ws[indicesOfCandKsToKeep]

  if(length(cand.Ks)==1){ #If there is only one candidate K after filtering, then there is no need for model selection.
    membership<-results[[1]]$membership
    toReturn<-list(K=cand.Ks,membership=membership)
    return(toReturn)
  }

  #Get model scores (AICs, AICcs, or BICs)
  if(length(mod.sel)>1) mod.sel<-mod.sel[1]
  modelScores<-sapply(results,FUN=function(result){
    membership<-result$membership
    return(getModelScore(x,y,membership,mod.sel))
  }) #Vector of length 4 (by default)

  #Choose K
  K<-cand.Ks[which.min(modelScores)]

  #If verbose, print "The K value chosen by AIC is 2." (if AIC is used and 2 is chosen)
  if(verbose) cat("The K value chosen by ",mod.sel," is ",K,".","\n",sep="")

  #If verbose, plot W and AIC against candidate K (if AIC is used)
  if(verbose){
    par(mfrow=c(2,1),mar=c(3,3,2,1)) #"mar" means margin
    #Top panel: Scree plot
    plot(cand.Ks,Ws,type="l",main="Scree plot")
    title(xlab="K",ylab="Avg. sq. perp./vert. dist.",line=2)
    abline(v=K,lty=2) #lyt means line type.
    #Bottom panel: Choose K by AIC (if AIC is used)
    plot(cand.Ks,modelScores,type="l",main=paste0("Choose K by ",mod.sel))
    title(xlab="K",ylab=mod.sel,line=2)
    abline(v=K,lty=2) #lyt means line type.
  }

  membership<-results[[which.min(modelScores)]]$membership
  toReturn<-list(K=K,membership=membership)
  return(toReturn)
}

getModelScore<-function(x,y,membership,mod.sel){
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

  numOfParams<-5*cand.K+(cand.K-1) #For each line, there are two mean parameters and three covariance parameters.
  logLik<-sum(log(marginal_dens))
  if(mod.sel=="AIC"){
    modScore<-2*numOfParams-2*logLik
  }else if(mod.sel=="AICc"){ #Correction for small sample sizes.
    #Wikipedia: When the sample size is small, there is a substantial probability that
    #AIC will select models that have too many parameters, i.e. that AIC will overfit.
    #To address such potential overfitting, AICc was developed:
    #AICc is AIC with a correction for small sample sizes.
    modScore<-2*numOfParams-2*logLik+(2*numOfParams^2+2*numOfParams)/(n-numOfParams-1)
  }else if(mod.sel=="BIC"){
    modScore<-numOfParams*log(n)-2*logLik
  }
  return(modScore)
}


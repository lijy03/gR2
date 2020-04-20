#Specified scenario
#If inference is false, then return a list of one item: estimate.
#If inference is true, then return a list of four items: estimate, conf.level, conf.int, and p.val.
gR2_Specified<-function(x,y,z,
                        inference,conf.level,gR2.pop,alternative,method){
  #Point estimate
  point_estimate<-get_Point_Estimate(x,y,z)
  toReturn1<-list(estimate=point_estimate)

  #No inference vs. inference
  if (!inference){
    #No inference
    return(toReturn1) #A list of one item: estimate
  }else{
    #Inference
    toReturn2<-gR2_Specified_Inference(x,y,z,
                                       conf.level,gR2.pop,alternative,method,
                                       point_estimate) #A list of three items: conf.level, conf.int, and p.val
    toReturn<-c(toReturn1,toReturn2)
    return(toReturn) #A list of four items: estimate, conf.level, conf.int, and p.val
  }
}

#z is not null.
#Returns sample gR2 (scalar, not in a list), which is a weighted average
get_Point_Estimate<-function(x,y,z){
  n<-length(x)
  z_uniq<-unique(z)

  results<-sapply(z_uniq,FUN=function(z_k){ #Apply function to each element of vector
    idx<-which(z==z_k)
    p_k<-length(idx)/n
    x_k<-x[idx]
    y_k<-y[idx]
    x_k_bar<-mean(x_k)
    y_k_bar<-mean(y_k)
    rho_2_k<-sum((x_k-x_k_bar)*(y_k-y_k_bar))^2 / (sum((x_k-x_k_bar)^2)*sum((y_k-y_k_bar)^2))
    return(c(p_k,rho_2_k))
  }) #2*K

  point_estimate<-sum(results[1,]*results[2,])
  return(point_estimate)
}

#z is not null.
#Returns a list of three items: conf.level, conf.int, and p.val
gR2_Specified_Inference<-function(x,y,z,
                                  conf.level,gR2.pop,alternative,method,
                                  point_estimate){
  #Get asymptotic variance (\gamma^2)
  if (method=="general"){
    asym_var<-get_asym_var_general(x,y,z)
  }else{
    asym_var<-get_asym_var_binorm(x,y,z)
  }

  #Calculate confidence interval
  n<-length(x)
  std<-sqrt(asym_var/n)
  tail.prob<-(1-conf.level)/2
  quantiles<-c(tail.prob,1-tail.prob)
  CI<-point_estimate+qnorm(quantiles)*std

  #Calculate p-value
  if(gR2.pop==0){ #If gR2.pop=0, then use the alternative hypothesis gR2.pop>0 regardless of alternative.
    p.val<-pnorm(point_estimate,mean=gR2.pop,sd=std,lower.tail=F)
  }else{ #If gR2.pop>0, then proceed as usual.
    if(alternative=="two.sided"){
      p.val1<-pnorm(point_estimate,mean=gR2.pop,sd=std,lower.tail=T)
      p.val2<-pnorm(point_estimate,mean=gR2.pop,sd=std,lower.tail=F)
      p.val<-2*min(p.val1,p.val2)
    }
    else if(alternative=="less"){
      p.val<-pnorm(point_estimate,mean=gR2.pop,sd=std,lower.tail=T)
    }
    else if(alternative=="greater"){
      p.val<-pnorm(point_estimate,mean=gR2.pop,sd=std,lower.tail=F)
    }
  }

  #Return
  toReturn<-list(conf.level=conf.level,conf.int=CI,p.val=p.val)
  return(toReturn)
}


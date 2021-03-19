#K is fixed.
#Returns a list of two items: membership and W
#W will be used for plotting when choosing K.
Klines<-function(x,y,
                 K,num_init,mc.cores,regressionMethod){
  if (K==1){
    # result<-Klines_1Cpp(x,y,regressionMethod)
    result<-Klines_1(x,y,regressionMethod)
  }else{
    result<-Klines_not_1(x,y,
                         K,num_init,mc.cores,regressionMethod)
    # if(!is.null(mc.cores)){
    #   RcppParallel::setThreadOptions(numThreads=mc.cores)
    # }
    # result<-Klines_not_1Cpp(x,y,
    #                         K,num_init,regressionMethod)

  }
  result$membership<-as.integer(as.vector(result$membership)) #Convert n*1 matrix to vector of length n, and convert numbers to integers
  return(result)
}

#K is fixed (K=1).
#Returns a list of two items: membership and W
Klines_1<-function(x,y,regressionMethod){
  n<-length(x)
  membership<-as.integer(rep(1,n)) #as.integer is necessary because otherwise it's a vector of numbers.

  #Get matrixOfDistances. Compare to update100Times().
  k<-1
  idx<-which(membership==k)
  x_k<-x[idx]
  y_k<-y[idx]
  if(var(y_k)>0){
    #Major axis regression vs. linear regression
    if(regressionMethod=="MA"){
      #lmodel2() will throw an error if variance in x_k is 0.
      suppressMessages(reg_res<-lmodel2::lmodel2(y_k~x_k)$regression.results)
      a<-reg_res$Slope[reg_res$Method=="MA"] #Slope
      b<-reg_res$Intercept[reg_res$Method=="MA"] #Intercept
    }else{
      #lm() will not throw an error if variance in x_k is 0,
      #but the slope will be reported as NA, which we recode as 0.
      reg_res<-lm(y_k~x_k)
      a<-reg_res$coefficients[2] #Slope
      b<-reg_res$coefficients[1] #Intercept
      if(is.na(a)){a<-0} #Edge case
    }
  }else{ #Edge case
    a<-0
    b<-y_k[1]
  }

  #Major axis regression vs. linear regression
  if(regressionMethod=="MA"){
    matrixOfDistances<-abs(a*x-y+b)/sqrt(a^2+1) #n*1
  }else{
    matrixOfDistances<-abs(y-(a*x+b)) #n*1
  }

  #Get W. Compare to Klines_each.
  W<-1/n*sum(matrixOfDistances^2)

  toReturn<-list(membership=membership,W=W)
  return(toReturn)
}

#K is fixed (K is not 1).
#Finds the line centers that minimizes W (average squared perpendicular/vertical distance) with num_init initializations
#Returns a list of two items: membership and W
Klines_not_1<-function(x,y,
                       K,num_init,mc.cores,regressionMethod){
  #Run Klines_each num_init times
  results<-parallel::mclapply(1:num_init,FUN=function(i){
    return(Klines_each(x,y,K,regressionMethod))
  },mc.cores=mc.cores) #results is a list (length is num_init) of lists (length is 2)
  Ws<-sapply(results,FUN=function(result){
    return(result$W)
  })
  idx<-which.min(Ws)
  return(results[[idx]])
}

#K is fixed (K is not 1).
#Finds the line centers that minimizes W (average squared perpendicular/vertical distance) with 1 initialization
#Returns a list of two items: membership and W
Klines_each<-function(x,y,K,regressionMethod){
  n<-length(x)

  #Randomly initialize
  membership<-sample(1:K,size=n,replace=TRUE)
  while (length(unique(membership))<K | any(table(membership)<3)){ #Edge case
    membership<-sample(1:K,size=n,replace=TRUE)
  }

  #Update 100 times
  result<-update100Times(x,y,K,membership,regressionMethod)
  membership<-result$membership
  #matrixOfDistances<-result$matrixOfDistances #Don't need to store matrixOfDistances the first time update100Times is called

  #EM update 100 times
  membership<-EMUpdate100Times(x,y,K,membership)

  #Update 100 times
  result<-update100Times(x,y,K,membership,regressionMethod)
  membership<-result$membership
  matrixOfDistances<-result$matrixOfDistances

  #Calculate W
  WEntires<-sapply(1:n,FUN=function(i){
    return(matrixOfDistances[i,membership[i]])
  })
  W<-1/n*sum(WEntires^2)

  return(list(membership=membership,W=W))
}

#Given the current membership assignment, update the assignment 100 times
#Returns a list of two items: membership, matrixOfDistances (perpendicular or vertical distances)
update100Times<-function(x,y,K,membership,regressionMethod){
  n<-length(x)

  for (i in 1:100){
    #Step 1, calculate the line centers based on the current membership assignment:
    lineCenters<-sapply(1:K,FUN=function(k){
      idx<-which(membership==k)
      x_k<-x[idx]
      y_k<-y[idx]
      if(var(y_k)>0){
        #Major axis regression vs. linear regression
        if(regressionMethod=="MA"){
          #lmodel2() will throw an error if variance in x_k is 0.
          suppressMessages(reg_res<-lmodel2::lmodel2(y_k~x_k)$regression.results)
          a<-reg_res$Slope[reg_res$Method=="MA"] #Slope
          b<-reg_res$Intercept[reg_res$Method=="MA"] #Intercept
        }else{
          #lm() will not throw an error if variance in x_k is 0,
          #but the slope will be reported as NA, which we recode as 0.
          reg_res<-lm(y_k~x_k)
          a<-reg_res$coefficients[2] #Slope
          b<-reg_res$coefficients[1] #Intercept
          if(is.na(a)){a<-0} #Edge case
        }
      }else{ #Edge case
        a<-0
        b<-y_k[1]
      }
      return(c(a,b))
    }) #2*K
    #Step 2, update membership assignment:
    #Get the matrix of distances (perpendicular or vertical distances)
    matrixOfDistances<-apply(lineCenters,2,FUN=function(beta){ #2 means by column
      a<-beta[1]
      b<-beta[2]
      #Major axis regression vs. linear regression
      if(regressionMethod=="MA"){
        toReturn<-abs(a*x-y+b)/sqrt(a^2+1) #n*1
      }else{
        toReturn<-abs(y-(a*x+b)) #n*1
      }
      return(toReturn)
    }) #n*K
    #Store the current membership in membershipOld
    membershipOld<-membership
    #Assign the points to the line centers
    membership<-max.col(-matrixOfDistances,"first")
    while(length(unique(membership))<K | any(table(membership)<3)){ #Edge case
      noise<-rnorm(n*K,mean=0,sd=sd(matrixOfDistances))
      noiseMatrix<-matrix(noise,nrow=n)
      matrixOfDistances<-matrixOfDistances-noiseMatrix #Changed. Fixed typo
      membership<-max.col(-matrixOfDistances,ties.method="first")
    }

    if(identical(membershipOld,membership)){
      break
    }
  }

  return(list(membership=membership,matrixOfDistances=matrixOfDistances))
} #End of update100Times

#Given the current membership assignment, update the assignment 100 times
#Returns membership
EMUpdate100Times<-function(x,y,K,membership){
  n<-length(x)
  data<-cbind(x,y)

  #Calculate a set of initial parameters
  parameters<-lapply(1:K,FUN=function(k){
    idx<-which(membership==k)
    p_k<-length(idx)/n
    data_k<-data[idx,]
    mu_k<-colMeans(data_k)
    Sigma_k<-cov(data_k)
    parameters_k<-list(p_k,mu_k,Sigma_k)
    return(parameters_k)
  }) #A list of K lists, each of length 3

  for(i in 1:100){
    #E-step
    #Calculate the n*K density matrix
    densityMatrix<-sapply(parameters,FUN=function(parameters_k){
      p_k<-parameters_k[[1]]
      mu_k<-parameters_k[[2]]
      Sigma_k<-parameters_k[[3]]
      density_k<-mvtnorm::dmvnorm(data,mean=mu_k,sigma=Sigma_k)
      density_k<-p_k*density_k
      return(density_k)
    }) #n*K

    #Get the n*K weight matrix kxi
    kxi<-densityMatrix/rowSums(densityMatrix) #n*K
    kxi[is.na(kxi)]<-0 #Edge case

    parametersOld<-parameters

    #M-step
    kxiColSums<-colSums(kxi) #Vector of length K
    p<-colSums(kxi)/n #Vector of length K
    mu<-t(kxi)%*%data/kxiColSums #Kx2
    parameters<-lapply(1:K,FUN=function(k){
      p_k<-p[k]
      mu_k<-mu[k,]
      sigma2_Xk<-kxi[,k]%*%(x-mu[k,1])^2/kxiColSums[k]
      sigma2_Yk<-kxi[,k]%*%(y-mu[k,2])^2/kxiColSums[k]
      sigma_XYk<-kxi[,k]%*%((x-mu[k,1])*(y-mu[k,2]))/kxiColSums[k]
      Sigma_k<-rbind(c(sigma2_Xk,sigma_XYk),c(sigma_XYk,sigma2_Yk))
      parameters_k<-list(p_k,mu_k,Sigma_k)
      return(parameters_k)
    }) #A list of K lists, each of length 3

    diff<-max(abs(unlist(parametersOld)-unlist(parameters)))
    if(is.na(diff)){ #Edge case
      diff<-0
    }
    if(diff<=0.001){
      break
    }
  } #End of for loop

  membership<-apply(kxi,1,FUN=function(kxiRow){ #1 means by row
    return(which.max(kxiRow))
  })
  while(length(unique(membership))<K | any(table(membership)<3)){ #Edge case
    noise<-rnorm(n*K,mean=0,sd=sd(kxi))
    noiseMatrix<-matrix(noise,nrow=n)
    kxi<-kxi-noiseMatrix #Changed. Fixed typo
    membership<-max.col(kxi,ties.method="first")
  }

  return(membership)
} #End of EMUpdate100Times




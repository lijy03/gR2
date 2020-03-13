#First, clean and rebuild package.

#This file shows that gR2 with Rcpp is reproducible.

#Unspecified scenario, K chosen, MA/LM
#Given a simulated data set and K, perform 1 comparisons
#100 simulated data sets total
#K varies from 1 to 4. So 25 simulated data sets for each K.

library(gR2)
RNGkind("L'Ecuyer-CMRG") #This has to do with random seeds in mclapply.
source("/home/heatherjzhou/2019.06.25_gR2/gR2/test/Functions1_simulateData.R")

fillResults<-function(results){
  for (i in 1:100){
    data<-simulateData(seed=i)
    x<-data[,1]
    y<-data[,2]
    K<-i%%4+1

    set.seed(7*i)
    result<-gR2(x,y,K=K,regressionMethod="MA")
    results[i,1]<-i
    results[i,2]<-K
    results[i,3]<-result$estimate
  }
  return(results)
}

results<-matrix(rep(NA,300),nrow=100)
results<-fillResults(results)

results2<-matrix(rep(NA,300),nrow=100)
results2<-fillResults(results2)

resultsCompare<-results2-results

#First, clean and rebuild package.

#This file show that gR2 agrees with gR2_No_Rcpp.

#Source files
if(TRUE){
  library(gR2)
  RNGkind("L'Ecuyer-CMRG") #This has to do with random seeds in mclapply, which is used in gR2_No_Rcpp.

  #Load gR2_No_Rcpp
  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/1_gR2.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/2_gR2_Specified.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/3_Inference_Functions.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/4_gR2_Unspecified.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/5_Klines.R")

  source("/home/heatherjzhou/2019.06.25_gR2/gR2/test/Functions1_simulateData.R")
  source("/home/heatherjzhou/2019.06.25_gR2/gR2/test/Functions2_Compare_to_Original_Code.R")
}

#1. Specified scenario. Success.
#100 simulated data sets
#For each simulated data set, 3 comparisons: no inference, inference (general), and inference (bivariate normal)
if(TRUE){
  comparisons<-matrix(rep(NA,300),nrow=100)
  for (i in 1:100){
    data<-simulateData(seed=7*i)
    x<-data[,1]
    y<-data[,2]
    z<-data[,3]

    result11<-gR2_No_Rcpp(x,y,z) #no inference
    result12<-gR2(x,y,z)
    result21<-gR2_No_Rcpp(x,y,z,inference=TRUE) #inference (general)
    result22<-gR2(x,y,z,inference=TRUE)
    result31<-gR2_No_Rcpp(x,y,z,inference=TRUE,method="binorm") #inference (bivariate normal)
    result32<-gR2(x,y,z,inference=TRUE,method="binorm")

    comparisons[i,]<-c(identical(result11,result12),identical(result21,result22),identical(result31,result32))
  }

  sum(comparisons) #300
}

#2. Unspecified scenario, K chosen, MA/LM. Success.
#Given a simulated data set and K, perform 1 comparison.
#100 simulated data sets total, K varies from 1 to 4.
#So 25 simulated data sets for each K.
if(TRUE){
  comparisons<-matrix(rep(NA,400),nrow=100)
  for (i in 1:100){
    K<-i%%4+1
    comparison<-comparisonUnspecifiedKChosen(seed=i,K=K,nstart=60,regressionMethod="MA")
    cat(comparison,"\n")
    comparisons[i,]<-comparison
  }

  comparisons<-as.data.frame(comparisons)
  colnames(comparisons)<-c("Seed","K","W_No_Rcpp_Minus_W","num_of_disagreeing_memberships")

  #MA
  W_difference<-comparisons$W_No_Rcpp_Minus_W
  sum(W_difference==0) #75. For 75 out of 100 times, the difference between W1 and W2 is 0.
  sum(W_difference>0) #13. For 13 out of 100 times, the difference between W1 and W2 is positive.
  sum(W_difference<0) #12. For 12 out of 100 times, the difference between W1 and W2 is negative.
  mean(W_difference) #-0.002575053. On average, W1 is slightly smaller than W2. But this can be explained by an outlier (see histogram).
  W_difference_nonzero<-W_difference[which(W_difference!=0)]
  hist(W_difference_nonzero)

  # #LM
  # W_difference<-comparisons$W_No_Rcpp_Minus_W
  # sum(W_difference==0) #88. For 88 out of 100 times, the difference between W1 and W2 is 0.
  # sum(W_difference>0) #7 For 7 out of 100 times, the difference between W1 and W2 is positive.
  # sum(W_difference<0) #5. For 5 out of 100 times, the difference between W1 and W2 is negative.
  # mean(W_difference) #2.980256e-05. On average, W1 is slightly greater than W2. So gR2_RcppParallel is no worse than gR2_No_Rcpp.
  # W_difference_nonzero<-W_difference[which(W_difference!=0)]
  # hist(W_difference_nonzero)
}

#3. Unspecified scenario, K not chosen, MA/LM. Success.
#Given a simulated data set and K, perform 3 comparisons: no inference, inference (general), and inference (bivariate normal)
#10 simulated data sets total
if(TRUE){
  comparisons<-matrix(rep(NA,90),nrow=10)
  for (i in 1:10){
    comparison<-comparisonUnspecifiedKNotChosen(seed=i,regressionMethod="MA")
    cat(i,comparison,"\n")
    comparisons[i,]<-comparison
  }

  comparisons<-as.data.frame(comparisons)
  colnames(comparisons)<-c("Estimate equal","K equal","Num of different membership assignments","Estimate equal","K equal","Num of different membership assignments","Estimate equal","K equal","Num of different membership assignments")
  View(comparisons)
}


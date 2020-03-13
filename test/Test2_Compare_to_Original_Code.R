#First, clean and rebuild package.

#Source files
if(TRUE){
  library(gR2)
  RNGkind("L'Ecuyer-CMRG") #This has to do with random seeds in mclapply.

  # source("/home/heatherjzhou/2019.06.25_gR2/2019.10.29_Improving_gR2/2018.11.25_Original_Implementation/functions.R")
  # source("/home/heatherjzhou/2019.06.25_gR2/2019.10.29_Improving_gR2/2018.11.25_Original_Implementation/gR2.R")
  # source("/home/heatherjzhou/2019.06.25_gR2/2019.10.29_Improving_gR2/2018.11.25_Original_Implementation/Klines.R")
  # source("/home/heatherjzhou/2019.06.25_gR2/2019.10.29_Improving_gR2/2018.11.25_Original_Implementation/R2gS.R")
  # source("/home/heatherjzhou/2019.06.25_gR2/2019.10.29_Improving_gR2/2018.11.25_Original_Implementation/R2gU.R")

  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/1_gR2.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/2_gR2_Specified.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/3_Inference_Functions.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/4_gR2_Unspecified.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/5_Klines.R")

  source("/home/heatherjzhou/2019.06.25_gR2/gR2/test/Functions1_simulateData.R")
  source("/home/heatherjzhou/2019.06.25_gR2/gR2/test/Functions2_Compare_to_Original_Code.R")
}

#1.Test that gR2 agrees with gR2_No_Rcpp for all MA scenarios. Success.

#1.1. Specified scenario. Success.
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

#1.2. Unspecified scenario, K chosen, MA/LM. Success.
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
  sum(W_difference==0) #78. For 78 out of 100 times, the difference between W1 and W2 is 0.
  sum(W_difference>0) #11. For 11 out of 100 times, the difference between W1 and W2 is positive.
  sum(W_difference<0) #11. For 11 out of 100 times, the difference between W1 and W2 is negative.
  mean(W_difference) #0.001089164 On average, W1 is slightly greater than W2. So my Rcpp code is definitely no worse than my code without Rcpp.
  W_difference_nonzero<-W_difference[which(W_difference!=0)]
  hist(W_difference_nonzero)

  # #LM
  # W_difference<-comparisons$W_No_Rcpp_Minus_W
  # sum(W_difference==0) #85. For 78 out of 100 times, the difference between W1 and W2 is 0.
  # sum(W_difference>0) #10. For 11 out of 100 times, the difference between W1 and W2 is positive.
  # sum(W_difference<0) #5. For 11 out of 100 times, the difference between W1 and W2 is negative.
  # mean(W_difference) #0.001767276 On average, W1 is slightly greater than W2. So my Rcpp code is definitely no worse than my code without Rcpp.
  # W_difference_nonzero<-W_difference[which(W_difference!=0)]
  # hist(W_difference_nonzero)
}

#1.3. Unspecified scenario, K not chosen, MA/LM. Success.
#Given a simulated data set and K, perform 3 comparisons: no inference, inference (general), and inference (bivariate normal)
#10 simulated data sets total
#Behavior is weird when MA, seed=3, but that's almost certainly a fluke.
if(TRUE){
  comparisons<-matrix(rep(NA,90),nrow=10)
  for (i in 1:10){
    comparison<-comparisonUnspecifiedKNotChosen(seed=i,regressionMethod="MA")
    cat(i,comparison,"\n")
    comparisons[i,]<-comparison
  }

  comparisons<-as.data.frame(comparisons)
  colnames(comparisons)<-c("Estimate equal","K equal","Num of different membership assignments","Estimate equal","K equal","Num of different membership assignments","Estimate equal","K equal","Num of different membership assignments")

  #Look at comparisons
}


#2. Test that gR2 produces reasonable results for all MA and LM scenarios. Success.
#Basic categories: specified, unspecified K chosen, unspecified K not chosen
#Inference: no inference, inference (general), inference (bivariate normal)
#If unspecified: MA, LM
#Total number of scenarios: 3(specified)+3(unspecified K chosen)+3(unspecified K not chosen)+6(LM)=15
#If specified + no inference, then output a list of one item: estimate.
#If specified + inference, then output a list of four items: estimate, conf.level, conf.int, and p.val.
#If unspecified + no inference, then output a list of three item: estimate, K, membership.
#If unspecified + inference, then output a list of six items: estimate, conf.level, conf.int, p.val, K, membership.
if(TRUE){
  data<-simulateData(seed=7)
  x<-data[,1]
  y<-data[,2]
  z<-data[,3]

  #Scenario 1: specified, no inference
  #Output: a list of one item: estimate
  result1<-gR2(x,y,z)
  result1

  #Scenario 2: specified, inference (general)
  #Output: a list of four items: estimate, conf.level, conf.int, and p.val
  result2<-gR2(x,y,z,inference=TRUE)
  result2

  #Scenario 3: specified, inference (bivariate normal)
  #Output: a list of four items: estimate, conf.level, conf.int, and p.val
  result3<-gR2(x,y,z,inference=TRUE,method="binorm")
  result3

  #Scenario 4: unspecified K chosen, no inference, MA
  #Output: a list of three item: estimate, K, membership
  result4<-gR2(x,y,K=2)
  result4

  #Scenario 5: unspecified K chosen, inference (general), MA
  #Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
  result5<-gR2(x,y,K=2,inference=TRUE)
  result5

  #Scenario 6: unspecified K chosen, inference (bivariate normal), MA
  #Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
  result6<-gR2(x,y,K=2,inference=TRUE,method="binorm")
  result6

  #Scenario 7: unspecified K not chosen, no inference, MA
  #Output: a list of three item: estimate, K, membership
  result7<-gR2(x,y)
  result7

  #Scenario 8: unspecified K not chosen, inference (general), MA
  #Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
  result8<-gR2(x,y,inference=TRUE)
  result8

  #Scenario 9: unspecified K not chosen, inference (bivariate normal), MA
  #Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
  result9<-gR2(x,y,inference=TRUE,method="binorm")
  result9

  #Scenario 10: unspecified K chosen, no inference, LM
  #Output: a list of three item: estimate, K, membership
  result10<-gR2(x,y,K=2,regressionMethod="LM")
  result10

  #Scenario 11: unspecified K chosen, inference (general), LM
  #Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
  result11<-gR2(x,y,K=2,regressionMethod="LM",inference=TRUE)
  result11

  #Scenario 12: unspecified K chosen, inference (bivariate normal), LM
  #Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
  result12<-gR2(x,y,K=2,regressionMethod="LM",inference=TRUE,method="binorm")
  result12

  #Scenario 13: unspecified K not chosen, no inference, LM
  #Output: a list of three item: estimate, K, membership
  result13<-gR2(x,y,regressionMethod="LM")
  result13

  #Scenario 14: unspecified K not chosen, inference (general), LM
  #Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
  result14<-gR2(x,y,regressionMethod="LM",inference=TRUE)
  result14

  #Scenario 15: unspecified K not chosen, inference (bivariate normal), LM
  #Output: a list of six items: estimate, conf.level, conf.int, p.val, K, membership
  result15<-gR2(x,y,regressionMethod="LM",inference=TRUE,method="binorm")
  result15

  #Lastly, check that the result is reasonable when cand.Ks is customized
  result16<-gR2(x,y,cand.Ks=1:5)
  result16
}
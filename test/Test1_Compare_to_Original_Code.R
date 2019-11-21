#First, clean and rebuild

#Import libraries and source files
if(TRUE){
  library(lmodel2)
  library(parallel)
  library(mvtnorm)
  RNGkind("L'Ecuyer-CMRG") #This has to do with random seeds in mclapply.

  source("/home/heatherjzhou/2019.06.25_gR2/2019.10.29_Improving_gR2/2018.11.25_Original_Implementation/functions.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.10.29_Improving_gR2/2018.11.25_Original_Implementation/gR2.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.10.29_Improving_gR2/2018.11.25_Original_Implementation/Klines.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.10.29_Improving_gR2/2018.11.25_Original_Implementation/R2gS.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.10.29_Improving_gR2/2018.11.25_Original_Implementation/R2gU.R")

  source("/home/heatherjzhou/2019.06.25_gR2/gR2/test/Functions1_Simulate_Data.R")
  source("/home/heatherjzhou/2019.06.25_gR2/gR2/test/Functions2_Compare_to_Original_Code.R")
}

#1.Test that gR2 agrees with gR2_Original for all MA scenarios. Success.
if(TRUE){
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

      result11<-gR2_Original(x,y,z) #no inference
      result12<-gR2(x,y,z)
      result21<-gR2_Original(x,y,z,inference=TRUE) #inference (general)
      result22<-gR2(x,y,z,inference=TRUE)
      result31<-gR2_Original(x,y,z,inference=TRUE,method="binorm") #inference (bivariate normal)
      result32<-gR2(x,y,z,inference=TRUE,method="binorm")

      comparisons[i,]<-c(identical(result11,result12),identical(result21,result22),identical(result31,result32))
    }

    sum(comparisons) #300
  }

  #1.2. Unspecified scenario, K chosen. Success.
  #Given a simulated data set and K, perform 3 comparisons: no inference, inference (general), and inference (bivariate normal)
  #100 simulated data sets total
  #K varies from 1 to 4. So 25 simulated data sets for each K.
  if(TRUE){
    comparisons<-matrix(rep(NA,300),nrow=100)
    for (i in 1:100){
      K<-i%%4+1
      comparison<-comparisonUnspecifiedKChosen(seed=i,K=K)
      cat(i,comparison,"\n")
      comparisons[i,]<-comparison
    }

    sum(comparisons) #300
  }

  #1.3. Unspecified scenario, K not chosen. Success.
  #Given a simulated data set and K, perform 3 comparisons: no inference, inference (general), and inference (bivariate normal)
  #10 simulated data sets total
  if(TRUE){
    comparisons<-matrix(rep(NA,30),nrow=10)
    for (i in 1:10){
      comparison<-comparisonUnspecifiedKNotChosen(seed=i)
      cat(i,comparison,"\n")
      comparisons[i,]<-comparison
    }

    sum(comparisons) #30
  }
}

#2. Test that gR2 produces reasonable results for all MA and LM scenarios. Success.
#Basic categories: specified, unspecified K chosen, unspecified K not chosen
#Inference: no inference, inference (general), inference (bivariate normal)
#If unspecified: MA, LM
#Total number of scenarios: 3+3+3+6=15
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
  result15<-gR2(x,y,cand.Ks=1:5)
  result15
}


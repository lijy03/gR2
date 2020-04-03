#First, clean and rebuild

#This file shows the amount of time
#it takes to run 100 times (K chosen, n=200).
#gR2_No_Rcpp:
#Time difference of 39.69904 secs (MA)
#Time difference of 22.34131 secs (LM)
#gR2_Rcpp:
#Time difference of 7.152082 secs (MA)
#Time difference of 7.277621 secs (LM)
#gR2 with RcppParallel:
#Time difference of 0.7682354 secs (MA)
#Time difference of 0.5530665 secs (LM)

#100 times (K not chosen, n=200)
#gR2_No_Rcpp:
#Time difference of 3.770518 mins, i.e., 226.2311 secs (MA)
#Time difference of 1.731112 mins, i.e., 103.8667 secs (LM)
#gR2_Rcpp:
#Time difference of 33.08457 secs (MA)
#Time difference of 33.07181 secs (LM)

if(TRUE){
  library(gR2)
  #RNGkind("L'Ecuyer-CMRG") #This has to do with random seeds in mclapply, which is used in gR2_No_Rcpp.

  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/1_gR2.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/2_gR2_Specified.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/3_Inference_Functions.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/4_gR2_Unspecified.R")
  source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/5_Klines.R")

  source("/home/heatherjzhou/2019.06.25_gR2/gR2/test/Functions1_simulateData.R")
}

if(TRUE){
  start_time<-Sys.time()
  for(i in 1:100){
    cat("i=",i,"\n",sep="")
    data<-simulateData(seed=i)
    x<-data[,1]
    y<-data[,2]
    set.seed(i)
    temp<-gR2(x,y,K=2,regressionMethod="MA")
    #temp<-gR2_No_Rcpp(x,y,K=2,regressionMethod="MA")
  }
  end_time<-Sys.time()
  end_time-start_time
}


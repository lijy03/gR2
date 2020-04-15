#First, clean and rebuild

#This file shows the amount of time to run gR2.

#100 times (K chosen, n=200)
if(TRUE){
  #gR2_No_Rcpp (K=2):
  #Time difference of 39.69904 secs (MA)
  #Time difference of 22.34131 secs (LM)
  #gR2_Rcpp (K=2):
  #Time difference of 7.152082 secs (MA)
  #Time difference of 7.277621 secs (LM)

  #gR2_RcppParallel (K=1,inference=FALSE):
  #Time difference of 0.1771374 secs (MA)
  #Time difference of 0.1535897 secs (LM)
  #gR2_RcppParallel (K=2,inference=FALSE):
  #Time difference of 0.7682354 secs (MA)
  #Time difference of 0.5530665 secs (LM)
  #gR2_RcppParallel (K=2,inference=TRUE):
  #Time difference of 0.8167498 secs (MA)
  #Time difference of 0.6874788 secs (LM)
  #gR2_RcppParallel (K=3,inference=FALSE):
  #Time difference of 1.299276 secs (MA)
  #Time difference of 1.245095 secs (LM)
  #gR2_RcppParallel (K=4,inference=FALSE):
  #Time difference of 1.695153 secs (MA)
  #Time difference of 1.642731 secs (LM)
}

#100 times (K not chosen, n=200)
if(TRUE){
  #gR2_No_Rcpp (cand.Ks=1:4):
  #Time difference of 3.770518 mins, i.e., 226.2311 secs (MA)
  #Time difference of 1.731112 mins, i.e., 103.8667 secs (LM)
  #gR2_Rcpp (cand.Ks=1:4):
  #Time difference of 33.08457 secs (MA)
  #Time difference of 33.07181 secs (LM)

  #gR2_RcppParallel (cand.Ks=1:2,inference=TRUE,verbose=FALSE):
  #Time difference of 0.9041815 secs (MA)
  #Time difference of 0.8250182 secs (LM)
  #gR2_RcppParallel (cand.Ks=1:3,inference=TRUE,verbose=FALSE):
  #Time difference of 2.113575 secs (MA)
  #Time difference of 1.91476 secs (LM)
  #gR2_RcppParallel (cand.Ks=1:4,inference=TRUE,verbose=FALSE):
  #Time difference of 4.011178 secs (MA)
  #Time difference of 3.625999 secs (LM)
}

#Source files
if(TRUE){
  library(gR2)
  #RNGkind("L'Ecuyer-CMRG") #This has to do with random seeds in mclapply, which is used in gR2_No_Rcpp.

  # #Load gR2_No_Rcpp
  # source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/1_gR2.R")
  # source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/2_gR2_Specified.R")
  # source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/3_Inference_Functions.R")
  # source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/4_gR2_Unspecified.R")
  # source("/home/heatherjzhou/2019.06.25_gR2/2019.11.20_gR2_Freeze_After_Sending_to_Jessica/R/5_Klines.R")

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
    #temp<-gR2(x,y,K=4,regressionMethod="MA")
    #temp<-gR2_No_Rcpp(x,y,K=2,regressionMethod="MA")
    temp<-gR2(x,y,cand.Ks=1:4,regressionMethod="LM",inference=TRUE,verbose=FALSE)
  }
  end_time<-Sys.time()
  end_time-start_time
}


#First, clean and rebuild

#Does the error message get printed when using the package? Yes.
if(TRUE){
  source("~/2019.06.25_gR2/gR2/test/Functions1_simulateData.R")
  data<-simulateData(seed=7)
  x<-data[,1]
  y<-data[,2]
  K=2
  temp<-gR2(x,y,K=2,regressionMethod="MA")

  x<-rep(1,200)
  y<-rep(2,200)
  temp2<-gR2(x,y,K=1,regressionMethod="MA")
}


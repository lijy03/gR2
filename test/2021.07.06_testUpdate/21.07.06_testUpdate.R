#First, clean and rebuild

library(gR2)
source("/home/heatherjzhou/2019.06.25_gR2/gR2/test/Functions1_simulateData.R")

data<-simulateData_eQTL(seed=7)
x<-data$x
y<-data$y
z<-data$z

result<-gR2(x,y,genotypeVector=x,cand.Ks=1:4,mod.sel="BIC")
result

#Test R syntax.
if(FALSE){
  #If a list is indexed as a vector is, then a list is returned.
  temp<-list(3,7,9,2) #A list of doubles.
  temp2<-temp[1] #A list of doubles.
  temp2<-temp[1:2] #A list of doubles.

  #To extract an element of the list, use double brackets.
  temp3<-temp[[1]]

  #Line plot smmoothes over missing x values.
  x<-c(1,2,4)
  y<-c(20,30,10)
  plot(x,y,type="l")

  #Line plot doesn't work when there is only one data point.
  x<-c(1)
  y<-c(20)
  plot(x,y,type="l")
}



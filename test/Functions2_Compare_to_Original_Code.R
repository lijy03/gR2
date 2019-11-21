comparisonUnspecifiedKChosen<-function(seed,K){
  toReturn<-rep(NA,3)

  data<-simulateData(seed=seed)
  x<-data[,1]
  y<-data[,2]
  z<-data[,3]

  #Unspecified, K=K, no inference
  set.seed(7*seed)
  result11<-gR2_Original(x,y,K=K)
  set.seed(7*seed)
  result12<-gR2(x,y,K=K)
  toReturn[1]<-identical(result11,result12)

  #Unspecified, K=K, inference (general)
  set.seed(7*seed)
  result21<-gR2_Original(x,y,K=K,inference=TRUE)
  set.seed(7*seed)
  result22<-gR2(x,y,K=K,inference=TRUE)
  toReturn[2]<-identical(result21,result22)

  #Unspecified, K=K, inference (binorm)
  set.seed(7*seed)
  result31<-gR2_Original(x,y,K=K,inference=TRUE,method="binorm")
  set.seed(7*seed)
  result32<-gR2(x,y,K=K,inference=TRUE,method="binorm")
  toReturn[3]<-identical(result31,result32)

  return(toReturn)
}

comparisonUnspecifiedKNotChosen<-function(seed){
  toReturn<-rep(NA,3)

  data<-simulateData(seed=seed)
  x<-data[,1]
  y<-data[,2]
  z<-data[,3]

  #Unspecified, K not chosen, no inference
  set.seed(7*seed)
  result11<-gR2_Original(x,y)
  set.seed(7*seed)
  result12<-gR2(x,y)
  toReturn[1]<-identical(result11,result12)

  #Unspecified, K not chosen, inference (general)
  result21<-gR2_Original(x,y,inference=TRUE)
  result22<-gR2(x,y,inference=TRUE)
  toReturn[2]<-identical(result21,result22)

  #Unspecified, K not chosen, inference (binorm)
  result31<-gR2_Original(x,y,inference=TRUE,method="binorm")
  result32<-gR2(x,y,inference=TRUE,method="binorm")
  toReturn[3]<-identical(result31,result32)

  return(toReturn)
}

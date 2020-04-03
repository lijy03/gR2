comparisonUnspecifiedKChosen<-function(seed,K,nstart,regressionMethod){
  toReturn<-rep(NA,3)

  data<-simulateData(seed=seed)
  x<-data[,1]
  y<-data[,2]
  z<-data[,3]

  #Unspecified scenario, K=K, no inference
  set.seed(7*seed)
  result11<-gR2_No_Rcpp(x,y,K=K,nstart=nstart,regressionMethod=regressionMethod)
  set.seed(7*seed)
  result12<-gR2(x,y,K=K,nstart=nstart,regressionMethod=regressionMethod)
  toReturn[1]<-seed
  toReturn[2]<-K
  toReturn[3]<-result11$estimate-result12$estimate
  toReturn[4]<-sum(result11$membership!=result12$membership)

  return(toReturn)
}

comparisonUnspecifiedKNotChosen<-function(seed,regressionMethod){
  # seed<-3
  # regressionMethod="MA"

  toReturn<-rep(NA,9)

  data<-simulateData(seed=seed)
  x<-data[,1]
  y<-data[,2]
  z<-data[,3]

  #Unspecified, K not chosen, no inference
  set.seed(seed)
  result11<-gR2_No_Rcpp(x,y,regressionMethod=regressionMethod)
  set.seed(seed)
  result12<-gR2(x,y,regressionMethod=regressionMethod)
  toReturn[1]<-identical(result11$estimate,result12$estimate)
  toReturn[2]<-result11$K==result12$K
  toReturn[3]<-sum(result11$membership!=result12$membership)

  # plot(x,y,col=z)
  # plot(x,y,col=result11$membership)
  # plot(x,y,col=result12$membership)

  #Unspecified, K not chosen, inference (general)
  set.seed(seed)
  result21<-gR2_No_Rcpp(x,y,inference=TRUE,regressionMethod=regressionMethod)
  set.seed(seed)
  result22<-gR2(x,y,inference=TRUE,regressionMethod=regressionMethod)
  toReturn[4]<-identical(result21$estimate,result22$estimate)
  toReturn[5]<-result21$K==result22$K
  toReturn[6]<-sum(result21$membership!=result22$membership)

  #Unspecified, K not chosen, inference (binorm)
  set.seed(seed)
  result31<-gR2_No_Rcpp(x,y,inference=TRUE,method="binorm",regressionMethod=regressionMethod)
  set.seed(seed)
  result32<-gR2(x,y,inference=TRUE,method="binorm",regressionMethod=regressionMethod)
  toReturn[7]<-identical(result31$estimate,result32$estimate)
  toReturn[8]<-result31$K==result32$K
  toReturn[9]<-sum(result31$membership!=result32$membership)

  return(toReturn)
}


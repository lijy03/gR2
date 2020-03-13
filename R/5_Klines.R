#K is fixed.
#Returns a list of two items: membership and W
#W will be used for plotting when choosing K.
Klines<-function(x,y,
                 K,num_init,mc.cores,regressionMethod){
  if (K==1){
    result<-Klines_1Cpp(x,y,regressionMethod)
  }else{
    result<-Klines_not_1(x,y,
                         K,num_init,mc.cores,regressionMethod)
  }
  result$membership<-as.integer(as.vector(result$membership)) #Convert n*1 matrix to vector of length n, and convert numbers to integers
  return(result)
}

#K is fixed (K is not 1).
#Finds the line centers that minimizes W (average squared perpendicular/vertical distance) with num_init initializations
#Returns a list of two items: membership and W
Klines_not_1<-function(x,y,
                       K,num_init,mc.cores,regressionMethod){
  #Run Klines_each num_init times
  results<-parallel::mclapply(1:num_init,FUN=function(i){
    return(Klines_eachCppOut(x,y,K,regressionMethod))
  },mc.cores=mc.cores) #results is a list (length is num_init) of lists (length is 2)
  Ws<-sapply(results,FUN=function(result){
    return(result$W)
  })
  idx<-which.min(Ws)
  return(results[[idx]])
}


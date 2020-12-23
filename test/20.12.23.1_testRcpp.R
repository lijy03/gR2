#First, clean and rebuild

Rcpp::sourceCpp("/home/heatherjzhou/2019.06.25_gR2/gR2/test/20.12.28_func1_Rcpp.cpp")

result<-myMaxCpp(1,2) #2

result<-testFunction1() #Inf
result<-testFunction2() #0

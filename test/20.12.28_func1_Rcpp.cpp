#include <RcppArmadillo.h>
using namespace arma;

//[[Rcpp::export()]]
float myMaxCpp(float a,float b){
  if(a>b){
    return a;
  }
  else{
    return b;
  }
}

//Test datum::inf
//[[Rcpp::export()]]
double testFunction1(){
  double W;
  W=datum::inf;
  return W;
}

//Test index_min()
//[[Rcpp::export()]]
uword testFunction2(){
  vec testVec(2);
  testVec(0)=datum::inf;
  testVec(1)=datum::inf;
  uword toReturn=testVec.index_min();
  return(toReturn);
}








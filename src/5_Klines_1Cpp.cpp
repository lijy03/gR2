#include <RcppArmadillo.h>
#include "1_getMatrixOfDistances.h"
using namespace arma;

//K is fixed (K=1).
//Returns a list of two items: membership and W
//[[Rcpp::export]]
Rcpp::List Klines_1Cpp(const arma::vec x,const arma::vec y,const std::string regressionMethod){
  const int n=x.n_elem;
  vec membership=ones<vec>(n);
  vec matrixOfDistances=getMatrixOfDistances(x,y,1,membership,regressionMethod); //Vector of length n
  double W=mean(square(matrixOfDistances)); //Compare to Klines_eachCpp
  return Rcpp::List::create(
    Rcpp::Named("membership")=membership, //Vector of length n
    Rcpp::Named("W")=W //Double
  );
}

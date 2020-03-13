#ifndef getMatrixOfDistances_h
#define getMatrixOfDistances_h

#include <RcppArmadillo.h>
using namespace arma;

arma::mat getMatrixOfDistances(const arma::vec x,const arma::vec y,const int K,arma::vec membership,const std::string regressionMethod);

#endif

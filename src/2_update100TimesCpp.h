#ifndef update100TimesCpp_h
#define update100TimesCpp_h

#include <RcppArmadillo.h>
using namespace arma;

struct Struct1 update100TimesCpp(const arma::vec x,const arma::vec y,const int K,arma::vec membership,const std::string regressionMethod);

#endif

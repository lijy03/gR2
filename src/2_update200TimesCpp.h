#ifndef update200TimesCpp_h
#define update200TimesCpp_h

#include <RcppArmadillo.h>
#include <dqrng_distribution.h>
using namespace arma;

struct Struct1 update200TimesCpp(const arma::vec x,const arma::vec y,
                                 const int K,arma::vec membership,const std::string regressionMethod,const dqrng::rng64_t rng,
                                 const arma::vec genotypeVector, const double minRelativeGroupSize);

#endif


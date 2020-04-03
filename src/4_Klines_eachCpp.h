#ifndef Klines_eachCpp_h
#define Klines_eachCpp_h

#include <RcppArmadillo.h>
#include <dqrng_distribution.h>
using namespace arma;

struct Struct2 Klines_eachCpp(const arma::vec x,const arma::vec y,
                              const int K,const std::string regressionMethod,const dqrng::rng64_t rng);

#endif


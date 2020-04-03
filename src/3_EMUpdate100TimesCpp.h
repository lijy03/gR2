#ifndef EMUpdate100TimesCpp_h
#define EMUpdate100TimesCpp_h

#include <RcppArmadillo.h>
#include <dqrng_distribution.h>
using namespace arma;

arma::vec EMUpdate100TimesCpp(const arma::vec x,const arma::vec y,
                              const int K,arma::vec membership,const dqrng::rng64_t rng);

#endif


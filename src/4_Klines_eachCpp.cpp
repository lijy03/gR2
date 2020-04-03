#include <RcppArmadillo.h>
#include <R_randgen.h> //For dqrng::R_random_int
#include <convert_seed.h> //For dqrng::convert_seed
#include <dqrng_distribution.h> //For dqrng::rng64_t
#include "0_Helper_Functions.h" //For Struct1 and Struct2
#include "2_update100TimesCpp.h"
#include "3_EMUpdate100TimesCpp.h"
using namespace arma;

//K is fixed (K is not 1).
//Finds the line centers that minimizes W (average squared perpendicular/vertical distance) with 1 initialization
//Returns an object storing two items: membership and W
struct Struct2 Klines_eachCpp(const arma::vec x,const arma::vec y,
                              const int K,const std::string regressionMethod,const dqrng::rng64_t rng){
  const int n=x.n_elem;
  vec membership=zeros<vec>(n); //Vector of length n, which will be randomly initialized and updated throughout the function.
  Struct1 result;
  mat matrixOfDistances=zeros<mat>(n,K); //n*K

  //Randomly initialize
  membership.imbue([&](){return (*rng)(uint32_t(K))+1;}); //Draw each entry from (1,...,K) with replacement (equal probabilities)
  //Edge case
  while(TRUE){
    vec groupSizes=getGroupSizes(membership,K);
    if(any(groupSizes<3)){
      membership.imbue([&](){return (*rng)(uint32_t(K))+1;}); //Draw each entry from (1,...,K) with replacement (equal probabilities)
    }else{
      break;
    }
  } //End of while(TRUE)

  //Update 100 times
  result=update100TimesCpp(x,y,
                           K,membership,regressionMethod,rng);
  membership=result.membership;
  //matrixOfDistances=result.matrixOfDistances; //Don't need to store matrixOfDistances the first time update100TimesRcpp() is called

  //EM update 100 times
  membership=EMUpdate100TimesCpp(x,y,
                                 K,membership,rng);

  //Update 100 times
  result=update100TimesCpp(x,y,
                           K,membership,regressionMethod,rng);
  membership=result.membership;
  matrixOfDistances=result.matrixOfDistances;

  //Calculate W
  vec WEntries=zeros<vec>(n);
  for(int i=0;i<n;i++){
    WEntries(i)=matrixOfDistances(i,membership(i)-1);
  }
  double W=mean(square(WEntries));

  Struct2 toReturn(membership,W);
  return toReturn;
}

// //[[Rcpp::export]]
// Rcpp::List Klines_eachCppOut(const arma::vec x,const arma::vec y,
//                              const int K,const std::string regressionMethod){
//   const uint64_t seed=dqrng::convert_seed<uint64_t>(Rcpp::IntegerVector(2,dqrng::R_random_int));
//   const dqrng::rng64_t rng=dqrng::generator<dqrng::xoroshiro128plus>(seed);
//
//   Struct2 result=Klines_eachCpp(x,y,K,regressionMethod,rng);
//
//   return Rcpp::List::create(
//     Rcpp::Named("membership")=result.membership,
//     Rcpp::Named("W")=result.W
//   );
// }


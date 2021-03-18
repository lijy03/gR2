// #include <RcppParallel.h>
// #include <RcppArmadillo.h>
// #include <R_randgen.h> //For dqrng::R_random_int
// #include <convert_seed.h> //For dqrng::convert_seed
// #include <dqrng_distribution.h> //For dqrng::rng64_t, dqrng::generator, and dqrng::xoroshiro128plus
// #include "0_Helper_Functions.h" //For Struct2
// #include "4_Klines_eachCpp.h"
// using namespace arma;
//
// struct KlinesEngine:public RcppParallel::Worker{
//   //Constant fields
//   RcppParallel::RVector<double> xRM; //Vector of length n. Do not set as constant so that later can use as an arma object
//   RcppParallel::RVector<double> yRM; //Vector of length n. Do not set as constant so that later can use as an arma object
//   const int K;
//   const std::string regressionMethod;
//   const uint64_t seed;
//   const int n; //Useful for operator()
//
//   //Fields to be updated
//   RcppParallel::RVector<double> WsRM; //Vector of length num_init
//   RcppParallel::RMatrix<double> membershipsRM; //n*num_init
//
//   //Constructor
//   KlinesEngine(const Rcpp::NumericVector xRcpp,const Rcpp::NumericVector yRcpp,
//                const int K,const std::string regressionMethod,const uint64_t seed,
//                Rcpp::NumericVector WsRcpp,Rcpp::NumericMatrix membershipsRcpp)
//     :xRM(xRcpp),yRM(yRcpp),
//      K(K),regressionMethod(regressionMethod),seed(seed),
//      n(xRcpp.length()),
//      WsRM(WsRcpp),membershipsRM(membershipsRcpp){}
//
//   void operator()(std::size_t begin,std::size_t end){
//     const dqrng::rng64_t rng=dqrng::generator<dqrng::xoroshiro128plus>(seed);
//     rng->seed(seed,end);
//     for(std::size_t initIndex=begin;initIndex<end;initIndex++){ //Set initIndex as std::size_t to avoid warning messages
//       const vec x=vec(xRM.begin(),n,false); //false means that no copying will be done.
//       const vec y=vec(yRM.begin(),n,false); //false means that no copying will be done.
//       Struct2 result=Klines_eachCpp(x,y,
//                                     K,regressionMethod,rng);
//       WsRM[initIndex]=result.W;
//       for (int obsIndex=0;obsIndex<n;obsIndex++){ //Fill the column in membershipsRM
//         membershipsRM(obsIndex,initIndex)=result.membership(obsIndex);
//       }
//     }
//   }
// };
//
// //K is fixed (K is not 1).
// //Finds the line centers that minimizes W (average squared perpendicular/vertical distance) with num_init initializations
// //Returns a list of two items: membership and W
// //[[Rcpp::export]]
// Rcpp::List Klines_not_1Cpp(const arma::vec x,const arma::vec y,
//                            const int K,const int num_init,const std::string regressionMethod){
//   const int n=x.n_elem;
//   //Store x and y in xRcpp and yRcpp, which is necessary for RcppParallel::Worker
//   const Rcpp::NumericVector xRcpp=Rcpp::NumericVector(x.begin(),x.end()); //Vector of length n
//   const Rcpp::NumericVector yRcpp=Rcpp::NumericVector(y.begin(),y.end()); //Vector of length n
//
//   //Generate a seed from R's RNG
//   const uint64_t seed=dqrng::convert_seed<uint64_t>(Rcpp::IntegerVector(2,dqrng::R_random_int));
//
//   //Initialize WsRcpp and membershipsRcpp, which will be updated when we call RcppParallel::parallelFor()
//   Rcpp::NumericVector WsRcpp=Rcpp::NumericVector(num_init); ///Vector of length num_init (filled with zeros)
//   Rcpp::NumericMatrix membershipsRcpp=Rcpp::NumericMatrix(n,num_init); //n*num_init (filled with zeros)
//
//   //Construct KlinesEngine functor
//   KlinesEngine klinesEngine(xRcpp,yRcpp,
//                             K,regressionMethod,seed,
//                             WsRcpp,membershipsRcpp);
//
//   //Fill WsRcpp and membershipsRcpp using RcppParallel
//   RcppParallel::parallelFor(0,num_init,klinesEngine);
//
//   //Get W and membership
//   vec Ws=vec(WsRcpp.begin(),num_init,false); //Vector of length num_init. false means that no copying will be done.
//   mat memberships=mat(membershipsRcpp.begin(),n,num_init,false); //n*num_init. false means that no copying will be done.
//
//   int idx=Ws.index_min();
//   double W=Ws(idx);
//   vec membership=memberships.col(idx);
//
//   return Rcpp::List::create(
//     Rcpp::Named("membership")=membership, //Vector of length n
//     Rcpp::Named("W")=W //Double
//   );
// }


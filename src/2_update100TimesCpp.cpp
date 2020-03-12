#include <RcppArmadillo.h>
#include "0_Helper_Functions.h"
#include "1_getMatrixOfDistances.h"
using namespace arma;

//Given the current membership assignment, update the assignment 100 times
//Returns an object storing two items: membership, matrixOfDistances (perpendicular or vertical distances)
struct Struct1 update100TimesCpp(const arma::vec x,const arma::vec y,const int K,arma::vec membership,const std::string regressionMethod){
  const unsigned int n=x.n_elem; //Set n as "unsigned" integer to avoid warning messages
  mat matrixOfDistances=zeros<mat>(n,K); //To be calculated and returned

  for(int i=0;i<100;i++){
    //Get matrix of distances,
    //which means based on the current membership assignment, calculate the distance from each point to each line center
    matrixOfDistances=getMatrixOfDistances(x,y,K,membership,regressionMethod);
    //Store the current membership in membershipOld
    vec membershipOld=membership;
    //Assign the points to the line centers
    membership=getMaxIndexByRow(-matrixOfDistances)+1;
    //Edge case
    while(TRUE){
      vec groupSizes=getGroupSizes(membership,K);
      if(any(groupSizes<3)){
        //Rcpp::Rcout<<"Random normal invoked in regular updates"<<endl;
        double sdOfMatrixOfDistances=stddev(vectorise(matrixOfDistances));
        mat noiseMatrix=sdOfMatrixOfDistances*randn(n,K);
        matrixOfDistances=matrixOfDistances+noiseMatrix; //Changed from Jessica's implementation and my implementation in R.
        membership=getMaxIndexByRow(-matrixOfDistances)+1;
      }else{
        break;
      }
    } //End of while(TRUE)

    if(sum(membershipOld==membership)==n){
      break;
    }

  } //End of for (int i=0;i<100;i++){

  Struct1 result(membership,matrixOfDistances);
  return result;
}

// //[[Rcpp::export]]
// Rcpp::List update100TimesOut(const arma::vec x,const arma::vec y,const int K,arma::vec membership,const std::string regressionMethod){
//   Struct1 result=update100TimesCpp(x,y,K,membership,regressionMethod);
//
//   return Rcpp::List::create(
//     Rcpp::Named("membership")=result.membership,
//     Rcpp::Named("matrixOfDistances")=result.matrixOfDistances
//   );
// }

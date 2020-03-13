#ifndef Helper_Functions_h
#define Helper_Functions_h

#include <RcppArmadillo.h>
using namespace arma;

struct Struct1{ //Return type of update100TimesCpp()
  //Fields
  arma::vec membership; //Vector of length n
  arma::mat matrixOfDistances; //n*K

  //Constructors
  Struct1(){};
  Struct1(arma::vec membership,arma::mat matrixOfDistances)
    :membership(membership),matrixOfDistances(matrixOfDistances){}
};

struct Struct2{ //Return type of Klines_eachCpp()
  //Fields
  arma::vec membership; //Vector of length n
  double W; //Double

  //Constructors
  Struct2(){};
  Struct2(arma::vec membership,double W)
    :membership(membership),W(W){}
};

arma::vec myMARegression(const arma::vec x,const arma::vec y);
arma::vec myFastLM(const arma::mat X,const arma::vec y);
arma::vec getMaxIndexByRow(const arma::mat matrix);
arma::vec getGroupSizes(const arma::vec membership,const int K);
arma::vec myDmvnorm(const arma::mat x,const arma::rowvec mean,const arma::mat sigma);

#endif

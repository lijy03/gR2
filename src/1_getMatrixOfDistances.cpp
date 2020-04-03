#include <RcppArmadillo.h>
#include "0_Helper_Functions.h"
using namespace arma;

//Given the current membership assignment (where each of the K groups has at least 3 data points),
//for each k, subset x and y, get a and b, get n*1 vector of distances, and put vector in matrixOfDistances
//Returns n*K matrixOfDistances
arma::mat getMatrixOfDistances(const arma::vec x,const arma::vec y,
                               const int K,arma::vec membership,const std::string regressionMethod){
  const int n=x.n_elem;
  mat matrixOfDistances=zeros<mat>(n,K); //To be calculated and returned

  for(int k=1;k<=K;k++){
    //For each k, subset x and y, get a and b, get n*1 vector of distances, and put vector in matrixOfDistances

    //1. Subset x and y
    uvec idx=find(membership==k);
    vec x_k=x.elem(idx);
    vec y_k=y.elem(idx);

    //2. Get a and b
    double a=0; //Slope. To be calculated and used
    double b=0; //Intercept. To be calculated and used
    double var_x_k=var(x_k);
    double var_y_k=var(y_k);
    if(var_x_k==0&&var_y_k==0){ //Edge case
      throw std::runtime_error("All data points in a group are exactly the same in update100TimesCpp().");
    }else if(var_x_k==0){ //Edge case
      if(regressionMethod.compare("MA")==0){
        throw std::runtime_error("Data points in a group have zero variance in x in update100TimesCpp().");
      }else{
        a=0; //Slope
        b=mean(y_k); //Intercept
      }
    }else if(var_y_k==0){ //Edge case
      a=0; //Slope
      b=y_k(0); //Intercept
    }else{
      if(regressionMethod.compare("MA")==0){
        //Major axis regression
        vec coef=myMARegression(x_k,y_k);
        a=coef(0); //Slope
        b=coef(1); //Intercept
      }else{
        //Linear regression
        int n_k=x_k.n_elem;
        vec vecOfOnes=ones<vec>(n_k);
        mat X_k=join_rows(vecOfOnes,x_k);
        vec coef=myFastLM(X_k,y_k);
        a=coef(1); //Slope
        b=coef(0); //Intercept
      }
    }

    //3. Get n*1 vector of distances
    vec vecOfDistances=zeros<vec>(n); //To be calculated and used
    if(regressionMethod.compare("MA")==0){
      vecOfDistances=abs(a*x-y+b)/sqrt(a*a+1); //square() doesn't work on a double.
    }else{
      vecOfDistances=abs(y-(a*x+b));
    }

    //4. Put vector in matrixOfDistances
    matrixOfDistances.col(k-1)=vecOfDistances;

  } //End of for (int k=1;k<=K;k++)

  return(matrixOfDistances);
}


#include <RcppArmadillo.h>
#include "0_Helper_Functions.h"
using namespace arma;

//Given the current membership assignment, update the assignment 100 times
//Returns membership
arma::vec EMUpdate100TimesCpp(const arma::vec x,const arma::vec y,const int K,arma::vec membership){
  const double n=x.n_elem; //Set n as a double to avoid rounding in divisions
  const mat data=join_rows(x,y); //n*2

  cube parameters=zeros<cube>(4,2,K); //4*2*K. To be calculated and used

  //Fill parameters with an initial set of parameters
  for(int k=1;k<=K;k++){
    uvec idx=find(membership==k);
    double p_k=idx.n_elem/n;
    mat data_k=data.rows(idx);
    rowvec mu_k=mean(data_k,0); //0 means by column. Returns a row vector of length 2
    mat Sigma_k=cov(data_k);

    mat parameters_k=zeros<mat>(4,2); //The first row is p_k and 0, the second row is mu_k, the third and forth rows are Sigma_k.
    parameters_k(0,0)=p_k;
    parameters_k.row(1)=mu_k;
    parameters_k.rows(2,3)=Sigma_k;
    parameters.slice(k-1)=parameters_k;
  }

  mat kxi=zeros<mat>(n,K); //n*K weight matrix. To be calculated and used

  //Update the initial parameters via EM
  for(int i=0;i<100;i++){
    //E-step
    //E-step Part 1: get n*K density matrix
    mat densityMatrix=zeros<mat>(n,K); //To be calculated and used
    for(int k=1;k<=K;k++){
      mat parameters_k=parameters.slice(k-1);
      double p_k=parameters_k(0,0);
      rowvec mu_k=parameters_k.row(1);
      mat Sigma_k=parameters_k.rows(2,3);
      densityMatrix.col(k-1)=p_k*myDmvnorm(data,mu_k,Sigma_k);
    }

    //E-step Part 2: get n*K weight matrix kxi
    kxi=densityMatrix.each_col()/sum(densityMatrix,1); //n*K
    kxi.replace(datum::nan,0); //Edge case

    cube parametersOld=parameters;

    //M-step. Fill parameters with new parameters
    rowvec kxiColSums=sum(kxi,0); //Row vector of length K
    rowvec p=kxiColSums/n; //Row vector of length K
    mat mu1=kxi.t()*data; //K*2
    mat mu=mu1.each_col()/kxiColSums.t(); //K*2
    for(int k=1;k<=K;k++){
      double p_k=p(k-1);
      rowvec mu_k=mu.row(k-1);
      double sigma2_Xk=dot(kxi.col(k-1),square(x-mu(k-1,0)))/kxiColSums(k-1);
      double sigma2_Yk=dot(kxi.col(k-1),square(y-mu(k-1,1)))/kxiColSums(k-1);
      double sigma2_XYk=dot(kxi.col(k-1),(x-mu(k-1,0))%(y-mu(k-1,1)))/kxiColSums(k-1);
      mat Sigma_k={{sigma2_Xk,sigma2_XYk},
      {sigma2_XYk,sigma2_Yk}};

      mat parameters_k=zeros<mat>(4,2); //The first row is p_k and 0, the second row is mu_k, the third and forth rows are Sigma_k.
      parameters_k(0,0)=p_k;
      parameters_k.row(1)=mu_k;
      parameters_k.rows(2,3)=Sigma_k;
      parameters.slice(k-1)=parameters_k;
    }

    double diff=0; //To be calculated and used
    if(parameters.has_nan()==false){
      diff=abs(parameters-parametersOld).max();
    }else{
      diff=0; //Edge case
    }
    if(diff<=0.001){
      break;
    }
  } //End of for(int i=0;i<100;i++)

  membership=getMaxIndexByRow(kxi)+1;

  //Edge case
  while(TRUE){
    vec groupSizes=getGroupSizes(membership,K);
    if(any(groupSizes<3)){
      //Rcpp::Rcout<<"Random normal invoked in EM updates"<<endl;
      double sdOfkxi=stddev(vectorise(kxi));
      mat noiseMatrix=sdOfkxi*randn(n,K);
      kxi=kxi+noiseMatrix; //Changed from Jessica's implementation and my implementation in R.
      membership=getMaxIndexByRow(kxi)+1; //Changed from Jessica's implementation and my implementation in R.
    }else{
      break;
    }
  } //End of while(TRUE)

  return membership;
}

#define ARMA_DONT_PRINT_ERRORS //This prevents chol() from giving a warning message when the decomposition fails.
#include <RcppArmadillo.h>
using namespace arma;

// Access an element:
// vector(i)
// matrix(i,j)
//
// Access a column or row of a matrix, or a slice of a cube:
// matrix.col(col_number)
// matrix.row(row_number)
// cube.slice(slice_number)

//Derived from the gR2 manuscript
arma::vec myMARegression(const arma::vec x,const arma::vec y){
  const mat X=join_rows(x,y); //n*2
  mat rotationMatrix;
  princomp(rotationMatrix,X); //Scaling is performed.
  double u11=rotationMatrix(0,0);
  double u12=rotationMatrix(1,0);
  double a=u12/u11;
  double b=-u12/u11*mean(x)+mean(y);
  vec coef={a,b};
  return coef;
}

arma::vec myFastLM(const arma::mat X,const arma::vec y){
  vec coef=solve(X,y);
  return coef;
}

arma::vec getMaxIndexByRow(const arma::mat matrix){
  const int numOfRows=matrix.n_rows;
  vec toReturn=zeros<vec>(numOfRows);
  for (int i=0;i<numOfRows;i++){
    toReturn[i]=matrix.row(i).index_max(); //When there is a tie, the first index is taken.
  }
  return toReturn;
}

arma::vec getGroupSizes(const arma::vec membership,const int K){
  vec toReturn=zeros<vec>(K);
  for(int k=1;k<=K;k++){
    toReturn[k-1]=size(find(membership==k))[0];
  }
  return toReturn;
}

const double log2pi=log(2*datum::pi);

//Function for getting multivariate normal density
//Adapted from https://gallery.rcpp.org/articles/dmvnorm_arma/
//Added edge case based on the implementation of mvtnorm::dmvnorm()
arma::vec myDmvnorm(const arma::mat x,const arma::rowvec mean,const arma::mat sigma){
  mat R;
  bool CholDecomposable=chol(R,sigma); //If the decomposition fails, chol(R,X) resets R and returns false.

  if(CholDecomposable==true){
    int n = x.n_rows;
    int xdim = x.n_cols;
    arma::vec out(n);
    //arma::mat rooti = arma::trans(arma::inv(trimatu(arma::chol(sigma))));
    arma::mat rooti = arma::trans(arma::inv(trimatu(R))); //Changed. Replaced line above
    double rootisum = arma::sum(log(rooti.diag()));
    double constants = -(static_cast<double>(xdim)/2.0) * log2pi;

    for (int i=0; i < n; i++) {
      arma::vec z = rooti * arma::trans( x.row(i) - mean) ;
      out(i)      = constants - 0.5 * arma::sum(z%z) + rootisum;
    }

    // if (logd == false) {
    //   out = exp(out);
    // }
    out = exp(out); //Changed. Replaced code above

    return(out);
  }else{ //Edge case
    //If sigma is not Cholesky decomposable, density is 0 by default.
    const int n=x.n_rows;
    vec toReturn=zeros<vec>(n); //n*1

    //However, if x is exactly equal to mean, density is infinity.
    mat absDiffFromMean=abs(x.each_row()-mean); //n*2
    vec totalAbsDiffFromMean=sum(absDiffFromMean,1); //n*1
    uvec whichEqualsMean=find(totalAbsDiffFromMean==0); //uvec
    vec newValues(whichEqualsMean.n_elem); newValues.fill(datum::inf); //whichEqualsMean.n_elem*1
    toReturn.elem(whichEqualsMean)=newValues;

    return toReturn;
  }

}

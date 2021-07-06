#include <RcppArmadillo.h>
#include <dqrng_distribution.h>
#include "0_Helper_Functions.h"
#include "1_getMatrixOfDistances.h"
using namespace arma;

//Given the current membership assignment, update the assignment 200 times
//Returns an object storing three items: membership, matrixOfDistances (perpendicular or vertical distances), and conditionsSatisfied
struct Struct1 update200TimesCpp(const arma::vec x,const arma::vec y,
                                 const int K,arma::vec membership,const std::string regressionMethod,const dqrng::rng64_t rng,
                                 const arma::vec genotypeVector, const double minRelativeGroupSize){
  const unsigned int n=x.n_elem; //Set n as "unsigned" integer to avoid warning messages
  mat matrixOfDistances=zeros<mat>(n,K); //To be calculated and returned
  bool conditionsSatisfied=true; //To be updated and returned

  for(int i=0;i<200;i++){
    //Get matrix of distances,
    //which means based on the current membership assignment, calculate the distance from each point to each line center
    matrixOfDistances=getMatrixOfDistances(x,y,
                                           K,membership,regressionMethod);
    //Store the current membership in membershipOld
    vec membershipOld=membership;
    //Assign the points to the line centers
    membership=getMaxIndexByRow(-matrixOfDistances)+1;
    //Edge case
    while(TRUE){
      vec groupSizes=getGroupSizes(membership,K);
      if(any(groupSizes<3)){
        //cout<<"Random normal invoked in regular updates"<<endl;
        double sdOfMatrixOfDistances=stddev(vectorise(matrixOfDistances));
        //mat noiseMatrix=sdOfMatrixOfDistances*randn(n,K); Replace with the following four lines to make reproducible
        mat noiseMatrix(n,K);
        dqrng::normal_distribution normalDist(0,1);
        noiseMatrix.imbue([&](){return normalDist(*rng);}); //Draw each entry from normal(0,1)
        noiseMatrix=sdOfMatrixOfDistances*noiseMatrix;

        matrixOfDistances=matrixOfDistances+noiseMatrix;
        membership=getMaxIndexByRow(-matrixOfDistances)+1;
      }else{
        break;
      }
    } //End of while(TRUE)

    if(sum(membershipOld==membership)==n){
      break;
    }

    //Check that every line-genotype combination contains
    //at least minRelativeGroupSize (default is 5%) of all people.
    for(int k=1;k<=K;k++){
      vec genotypeVectorSubgroup=genotypeVector(find(membership==k)); //Genotype vector for people in the kth line, length<=n.
      int numOfPeopleWithGenotype0=size(find(genotypeVectorSubgroup==0))[0]; //find() returns a vector of indices; size() returns a vector.
      int numOfPeopleWithGenotype1=size(find(genotypeVectorSubgroup==1))[0];
      int numOfPeopleWithGenotype2=size(find(genotypeVectorSubgroup==2))[0];
      //int numOfPeople=size(genotypeVectorSubgroup)[0];
      if(numOfPeopleWithGenotype0<minRelativeGroupSize*n
         ||numOfPeopleWithGenotype1<minRelativeGroupSize*n
         ||numOfPeopleWithGenotype2<minRelativeGroupSize*n){
        conditionsSatisfied=false;
        break;
      }
    }
    if(conditionsSatisfied==false){
      break;
    }

  } //End of for (int i=0;i<200;i++){

  Struct1 result(membership,matrixOfDistances,conditionsSatisfied);
  return result;
}


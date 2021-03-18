#' gR2
#'
#' \code{gR2} calculates the sample gR2 under the specified scenario, the unspecified scenario (K chosen), and the unspecified scenario (K not chosen). It also provides an option to perform statistical inference on the population gR2.
#'
#' The arguments that require user input are \code{x} and \code{y}, which must be numeric vectors of the same length.
#'
#' There are three broad types of scenarios: the specified scenario, the unspecified scenario (K chosen), and the unspecified scenario (K not chosen). The specified scenario is considered when \code{z} is provided; the unspecified scenario (K chosen) is considered when \code{z} is not provided but \code{K} is provided; and the unspecified scenario (K not chosen) is considered when neither \code{z} or \code{K} is provided.
#'
#' In the unspecified scenario (K chosen), we recommend that users set \code{K} to be less than or equal to \code{4} for interpretability.
#'
#' In the unspecified scenario (K not chosen), the \code{gR2} function automatically chooses a \code{K} value from \code{cand.Ks} using the Akaike information criterion (AIC). Two plots are outputted unless \code{verbose} is set to \code{FALSE}: (1) a scree plot that shows how average squared perpendicular/vertical distance changes with the candidate \code{K}, and (2), a plot that shows how AIC changes with the candidate \code{K}. Users can decide whether the \code{K} value chosen by the \code{gR2} function is reasonable by checking these two plots.
#'
#' @param x A numeric vector.
#' @param y A numeric vector of the same length as \code{x}.
#' @param z A vector of integers that represents the line membership of all the data points. Must be of the same length as \code{x} and \code{y}. Default is \code{NULL}.
#' @param K Number of lines in the unspecified scenario. Default is \code{NULL}.
#' @param cand.Ks A vector of positive integers that represents the candidate K’s in the unspecified scenario. Default is \code{1:4}.
#' @param nstart Number of initializations for the K-lines algorithm in the unspecified scenario. Default is \code{30}.
#' @param mc.cores Number of cores to use in the unspecified scenario. Default is the number of CPU cores minus one.
#' @param regressionMethod Valid values are \code{‘MA’} and \code{‘LM’}. Indicates which regression method to use in the K-lines algorithm - major axis regression or linear regression. Default is \code{‘MA’}.
#' @param verbose Logical. If \code{TRUE}, then messages are printed and a graph is produced in the unspecified scenario (K not chosen). Default is \code{TRUE}.
#' @param inference Logical. If \code{TRUE}, then the function calculates a confidence interval for the population gR2 of confidence level \code{conf.level}, as well as a p-value of the hypothesis test where the null hypothesis is that the population gR2 is equal to \code{gR2.pop}. Default is \code{FALSE}.
#' @param conf.level The confidence level of the confidence interval. Default is \code{0.95}.
#' @param gR2.pop The population gR2 in the null hypothesis of the hypothesis test. Must be between \code{0} and \code{1}. Default is \code{0}.
#' @param alternative Valid values are \code{‘two.sided’}, \code{‘less’}, and \code{‘greater’}. Indicates the type of the alternative hypothesis in the hypothesis test.
#' @param method Valid values are \code{‘general’} and \code{‘binorm’}. Indicates which asymptotic distribution of the sample gR2 to use for inference. Default is \code{‘general’}.
#' @param details Logical. If \code{TRUE}, then detailed information about each line (R squared and its corresponding p-value) is outputted. Only available in the unspecified scenario. Default is \code{FALSE}.
#'
#' @return \code{gR2} returns a list consisting of one or more of the following items:
#' \item{estimate}{The sample gR2.}
#' \item{conf.level}{The confidence level of the confidence interval (if \code{inference} is \code{TRUE}).}
#' \item{conf.int}{The confidence interval for the population gR2 (if \code{inference} is \code{TRUE}).}
#' \item{p.val}{The p-value of the hypothesis test where the null hypothesis is that the population gR2 is equal to \code{gR2.pop} and the alternative hypothesis is that the population gR2 is not equal to, less than, or greater than \code{gR2.pop} depending on \code{alternative} (if \code{inference} is \code{TRUE}).}
#' \item{K}{The number of lines in the unspecified scenario, either chosen by the user or chosen from \code{cand.Ks} by the \code{gR2} function.}
#' \item{membership}{The inferred line membership of all the data points in the unspecified scenario.}
#' \item{perLineInfo}{A matrix with three columns: lineIndex, R2, and pValue. Each row corresponds to a line. Total number of rows is \code{K}, the number of lines chosen.}
#'
#' @author Jingyi Jessica Li, \email{jli@stat.ucla.edu}
#' @author Heather J Zhou, \email{heatherjzhou@ucla.edu}
#'
#' @references
#' Li, J.J., Tong, X., and Bickel, P.J. (2019). Generalized R2 Measures for a Mixture of Bivariate Linear Dependences. arXiv.
#'
#' @export

gR2<-function(x,y,z=NULL, #basic arguments
              K=NULL,cand.Ks=1:4,nstart=30,mc.cores=parallel::detectCores()-1,regressionMethod="MA",verbose=TRUE, #Arguments for unspecified scenario
              inference=FALSE,conf.level=0.95,gR2.pop=0,alternative="greater",method="general", #Arguments for inference
              details=FALSE #Additional arguments
              ){

  #Check inputs
  if(length(x)!=length(y)){stop("x and y must be of the same length.")}
  if(!is.null(z)){
    if(length(z)!=length(x)){stop("z must be of the same length as x and y.")}
    if(!is.null(K)){warning("Since z is not null, K will not be used.")}
  }

  #Specified vs. unspecified
  if (!is.null(z)){
    #Specified scenario
    #If inference is false, then return a list of one item: estimate.
    #If inference is true, then return a list of four items: estimate, conf.level, conf.int, and p.val.
    toReturn<-gR2_Specified(x,y,z,
                            inference,conf.level,gR2.pop,alternative,method)
    return(toReturn)
  }else{
    #Unspecified scenario
    #First,
    #if K is chosen, get membership.
    #If K is not chosen, choose K and get membership (and print explanations along the way if verbose).
    #Then,
    #if inference is false, then return a list of three item: estimate, K, membership.
    #If inference is true, then return a list of six items: estimate, conf.level, conf.int, p.val, K, membership.
    #Also, if details is true, then include perLineInfo in the list as well.
    toReturn<-gR2_Unspecified(x,y,
                              K,cand.Ks,num_init=nstart,mc.cores,regressionMethod,verbose,
                              inference,conf.level,gR2.pop,alternative,method,
                              details)
    return(toReturn)
  }
}


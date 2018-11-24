#' Estimate population generalized R square measures
#'
#' \code{gR2} is the main function that calculates the sample generalized R square measures, i.e., point estimates, for the supervised scenario and the unsupervised scenario with or without K specified. It also performs statistical inference for the population measures, i.e., parameters of interest.
#'
#' \code{x}, \code{y} and \code{z} (if not \code{NULL}) must be numeric vectors of the same length. If \code{z} is not \code{NULL}, the supervised scenario is considered; otherwise, the unsupervised scenario is considered.
#'
#' Under the supervised scenario, \code{K} will not be used.
#'
#' Under the unsupervised scenario, if \code{is.null(K)} is \code{FALSE}, \code{K} must be a positive integer. If users are interested in the existence of a mixture of two linear relationships, set \code{K=2}. We do not recommend specifying \code{K} larger than \code{4} for interpretability consideration. If \code{is.null(K)} is \code{TRUE}, the function will automatically choose a \code{K} value from candidate values (default set to \code{{1,2,3,4}}) using the Akaike information criterion (AIC). The function will output two plots: (1) a scree plot showing how the within-cluster average squared perpendicular distances vary with \code{K} values, and (2) a plot showing how AIC changes with \code{K}. Users can decide whether the chosen \code{K} value is reasonable by checking these two plots.
#'
#' If \code{inference} is \code{FALSE} (default), the function only outputs a point estimate, i.e., a sample generalized R square measure. Under the unsupervised scenario, the inferred line memberships by the K-lines clustering will also be output. If \code{inference} is \code{TRUE}, the function will additionally output a \code{conf.level}-level confidence interval of the population generalized R square measure, as well as a p-value for a one-sided test against the null hypothesis that the population generalized R square is equal to zero.
#'
#' @param x a numeric vector.
#' @param y a numeric vector of the same length as \code{x}.
#' @param z an optional numeric vector containing integer values that indicate line memberships under the supervised scenario. The length of \code{z} must be the same as that of \code{x} and \code{y}.
#' @param K an optional number of lines under the unsupervised scenario, when \code{z=NULL}.
#' @param cand.Ks \code{1:4} (default) or a numeric vector containing the candidate values of \code{K}, when \code{K=NULL} under the unsupervised scenario (\code{z=NULL}).
#' @param inference logical. Should statistical inference be performed? Default is \code{FALSE}. If \code{TRUE}, a confidence interval of level \code{conf.level} will be computed for the population generalized R square measure, and a p-value will be computed for a one-sided test (greater than) against the null hypothesis that the population generalized R square measure is equal to zero.
#' @param conf.level the confidence level for the returned confidence interval. Default is \code{0.95}.
#' @param method a character string indicating which asymptotic distribution of the sample generalized measure is to be used for the inference. It must be one of \code{"general"} (the general asymptotic distribution) or \code{"binorm"} (the asymptotic distribution under the binormal distribution). The default is \code{"general"}.
#' @param nstart the number of initial starts for the K-lines algorithm under the unsupervised scenario (\code{z=NULL}).
#' @param mc.cores the number of cores to use, i.e. at most how many child processes will be run simultaneously. Must be at least one, and parallelization requires at least two cores. The default is the number of CPU cores minus one.
#' @return dd

gR2 <- function(x, y, z=NULL, K=NULL, # basic arguments
                cand.Ks=1:4, # arguments for choosing K
                inference=FALSE, conf.level = 0.95, method=c("general", "binorm"), # arguments for inference
                nstart=30, mc.cores=detectCores()-1 # arguments for the K-lines algorithm
                ) {
  n <- length(x)
  if (length(y) != n) {
    stop("Error: x and y have different lengths.")
  }
  if (!is.null(z)) {
    # the supervised scenario
    if (length(z) != n) {
      stop("Error: x and z have different lengths.")
    }
    if (!is.null(K)) {
      warning("Warning: this is the supervised scenario. K will not be used.")
    }
    if (!inference) {
      # point estimate only
      return(list(estimate=R2gS(x, y, z)))
    } else {
      # inference
      K <- length(unique(z))
      tail.prob <- (1-conf.level)/2 # left and right tail probability
      qts <- c(tail.prob, 1-tail.prob) # left and right CDFs
      if (length(method)>1) method <- method[1]
      if (method == "binorm") {
        param_est_binorm <- sup_param_est_binorm(x, y, z)
        avars_plugin_binorm <- asym_var_binorm(K, param_est_binorm$p_s, param_est_binorm$rho2_s) / n
        rho2gs_plugin <- sup_pop_R2g(K, param_est_binorm$p_s, param_est_binorm$mu_s, param_est_binorm$Sigma_s)
        CI_plugin <- rho2gs_plugin + qnorm(qts) * sqrt(avars_plugin_binorm) # CI
        p.val <- pnorm(rho2gs_plugin, mean=0, sd=sqrt(avars_plugin_binorm), lower.tail=F)
      } else {
        param_est_general <- sup_param_est_general(x, y, z)
        avars_plugin_general <- asym_var_general(K, param_est_general$p_s, param_est_general$rho_s, param_est_general$muX4_s, param_est_general$muY4_s, param_est_general$muX3Y_s, param_est_general$muXY3_s, param_est_general$muX2Y2_s) / n
        rho2gs_plugin <- sup_pop_R2g(K, param_est_general$p_s, param_est_general$mu_s, param_est_general$cov_s)
        CI_plugin <- rho2gs_plugin + qnorm(qts) * sqrt(avars_plugin_general)
        p.val <- pnorm(rho2gs_plugin, mean=0, sd=sqrt(avars_plugin_general), lower.tail=F)
      }
      return(list(
        estimate=rho2gs_plugin,
        conf.level=conf.level,
        conf.int=CI_plugin,
        p.val=p.val
      ))
    }
  } else {
    # the unsupervised scenario
    library(parallel)
    if (n < 50) {
      nstart <- floor(1500 / n)
    }
    if (is.null(K)) {
      library(mvtnorm)
      # K unspecified
      # run K-lines clustering for a range of K values
      Ks <- cand.Ks
      cat(paste("Candidate K values:", paste(Ks, collapse=", "), "\n"))
      results <- mclapply(Ks, FUN=function(K.cand) {
        Klines(x, y, K.cand, ifchooseK=T, num_init=nstart)
      }, mc.cores=mc.cores)
      # calculate the average within-cluster squared perpendicular distances & AICs
      metrics <- sapply(results, FUN=function(x) {
        D <- x$D
        membership <- x$membership
        W <- mean( sapply(1:n, FUN=function(i) {
          D[i, membership[i]]^2
        }) )
        K.cand <- length(unique(membership))
        joint_dens <- sapply(1:K.cand, FUN=function(k) {
          idx <- which(membership==k)
          mu_k <- c( mean(data$x[idx]), mean(data$y[idx]) )
          x_c <- data$x[idx] - mu_k[1]
          y_c <- data$y[idx] - mu_k[2]
          mat_c <- rbind(x_c, y_c)
          Sigma_k <- mat_c %*% t(mat_c) / length(idx)
          p_k <- length(idx) / n
          dmvnorm(cbind(data$x, data$y), mean=mu_k, sigma=Sigma_k, log = FALSE) * p_k
        }) # an nxK matrix, the joint densities of (X_i, Y_i, Z_i)
        mar_dens <- rowSums(joint_dens) # an n-dim vector, the marginal densities of (X_i, Y_i)
        # the AIC
        AIC <- 12 * K.cand - 2 * sum(log(mar_dens))
        return(c(W, AIC))
      }) # the first row contains Ws; the second row contains AICs
      Ws <- metrics[1,]
      AICs <- metrics[2,]
      K <- Ks[which.min(AICs)]
      # plot the choose K results
      par(mfrow=c(2,1), mar=c(3,3,2,1))
      plot(Ks, Ws, type="l", xlab="", ylab="", main="scree plot")
      title(xlab="K", ylab="Within-cluster sq. dist.", line=2)
      abline(v=K, lty=2)

      plot(Ks, AICs, type="l", xlab="", ylab="", main="choose K by AIC")
      title(xlab="K", ylab="AIC", line=2)
      abline(v=K, lty=2)
      abline(v=K, lty=2)

      cat(paste("The K value chosen by AIC is", K, ".\n"))
    }
    if (!inference) {
      # point estimate only
      result <- R2gU(x, y, K, num_init=nstart, mc.cores)
      return(list(
        estimate=result$estimate,
        K=K,
        membership=result$membership
      ))
    } else {
      # inference
      tail.prob <- (1-conf.level)/2 # left and right tail probability
      qts <- c(tail.prob, 1-tail.prob) # left and right CDFs
      if (length(method)>1) method <- method[1]
      z <- Klines(x, y, K, num_init=nstart, mc.cores=mc.cores)
      if (method == "binorm") {
        param_est_binorm <- sup_param_est_binorm(x, y, z)
        avaru_plugin_binorm <- asym_var_binorm(K, param_est_binorm$p_s, param_est_binorm$rho2_s) / n
        rho2gu_plugin <- sup_pop_R2g(K, param_est_binorm$p_s, param_est_binorm$mu_s, param_est_binorm$Sigma_s)
        CI_plugin <- rho2gu_plugin + qnorm(qts) * sqrt(avaru_plugin_binorm) # CI
        p.val <- pnorm(rho2gu_plugin, mean=0, sd=sqrt(avaru_plugin_binorm), lower.tail=F)
      } else {
        param_est_general <- sup_param_est_general(x, y, z)
        avaru_plugin_general <- asym_var_general(K, param_est_general$p_s, param_est_general$rho_s, param_est_general$muX4_s, param_est_general$muY4_s, param_est_general$muX3Y_s, param_est_general$muXY3_s, param_est_general$muX2Y2_s) / n
        rho2gu_plugin <- sup_pop_R2g(K, param_est_general$p_s, param_est_general$mu_s, param_est_general$cov_s)
        CI_plugin <- rho2gu_plugin + qnorm(qts) * sqrt(avaru_plugin_general)
        p.val <- pnorm(rho2gu_plugin, mean=0, sd=sqrt(avaru_plugin_general), lower.tail=F)
      }
      return(list(
        estimate=rho2gu_plugin,
        conf.level=conf.level,
        conf.int=CI_plugin,
        p.val=p.val,
        K=K,
        membership=z
      ))
    }
  }
}

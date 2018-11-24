# estimate parameters in the supervised scenario (binormal distribution)
sup_param_est_binorm <- function(x, y, z) {
  z_uniq <- unique(z)
  n <- length(z)
  results <- lapply(z_uniq, FUN=function(z_k) {
    idx <- which(z==z_k)
    p_k <- length(idx)/n
    data_k <- cbind(x[idx], y[idx])
    mu_k <- colMeans(data_k)
    Sigma_k <- var(data_k)

    rho2_k <- cor(data_k[,1], data_k[,2])^2

    return( list( p_k=p_k, mu_k=mu_k, Sigma_k=Sigma_k, rho2_k=rho2_k ) )
  })
  list(
    p_s = sapply(results, FUN=function(x) x$p_k),
    mu_s = lapply(results, FUN=function(x) x$mu_k),
    Sigma_s = lapply(results, FUN=function(x) x$Sigma_k),
    rho2_s = sapply(results, FUN=function(x) x$rho2_k)
  )
}

# estimate parameters in the supervised scenario (general)
sup_param_est_general <- function(x, y, z) {
  z_uniq <- unique(z)
  n <- length(z)
  results <- lapply(z_uniq, FUN=function(z_k) {
    idx <- which(z==z_k)
    p_k <- length(idx)/n
    data_k <- cbind(x[idx], y[idx])
    mu_k <- colMeans(data_k)
    cov_k <- var(data_k)

    rho_k <- cor(data_k[,1], data_k[,2])

    x_std <- ( x[idx] - mean(x[idx]) ) / sd(x[idx])
    y_std <- ( y[idx] - mean(y[idx]) ) / sd(y[idx])

    return( list( p_k=p_k, mu_k=mu_k, cov_k=cov_k, rho_k=rho_k, muX4_k=mean(x_std^4), muY4_k=mean(y_std^4), muX3Y_k=mean(x_std^3 * y_std), muXY3_k=mean(x_std * y_std^3), muX2Y2_k=mean(x_std^2 * y_std^2) ) )
  })
  list(
    p_s = sapply(results, FUN=function(x) x$p_k),
    mu_s = lapply(results, FUN=function(x) x$mu_k),
    cov_s = lapply(results, FUN=function(x) x$cov_k),
    rho_s = sapply(results, FUN=function(x) x$rho_k),
    muX4_s = sapply(results, FUN=function(x) x$muX4_k),
    muY4_s = sapply(results, FUN=function(x) x$muY4_k),
    muX3Y_s = sapply(results, FUN=function(x) x$muX3Y_k),
    muXY3_s = sapply(results, FUN=function(x) x$muXY3_k),
    muX2Y2_s = sapply(results, FUN=function(x) x$muX2Y2_k)
  )
}

# calculation of the asymptotic variance of \sqrt{n} * (sample R2g - pop R2g) in the general case
asym_var_general <- function(K, p_s, rho_s, mu_X4_s, mu_Y4_s, mu_X3Y_s, mu_XY3_s, mu_X2Y2_s) {
  A_s <- p_s * ( rho_s^4 * (mu_X4_s + 2*mu_X2Y2_s + mu_Y4_s) - 4 * rho_s^3 * (mu_X3Y_s + mu_XY3_s) + 4 * rho_s^2 * mu_X2Y2_s )
  B_s <- p_s * (1 - p_s) * rho_s^4
  C_s <- (-1) * outer(p_s*rho_s^2, p_s*rho_s^2)
  diag(C_s) <- 0
  sum(A_s) + sum(B_s) + sum(C_s)
}

# calculation of the asymptotic variance of \sqrt{n} * (sample R2g - pop R2g) in the bivariate Normal case
asym_var_binorm <- function(K, p_s, rho2_s) {
  A_s <- 4 * p_s * rho2_s * (1 - rho2_s)^2
  B_s <- p_s * (1 - p_s) * rho2_s^2
  C_s <- (-1) * outer(p_s*rho2_s, p_s*rho2_s)
  diag(C_s) <- 0
  sum(A_s) + sum(B_s) + sum(C_s)
}

# calculation of the supervised population generalized R2: \rho^2_{G(S)}
sup_pop_R2g <- function(K, p_s, mu_s, Sigma_s) {
  # K: number of lines
  # p_s: a numeric vector of length K; probabilities that sum up to 1
  # mu_s: a list of K mean vectors, each with dimension 2
  # Sigma_s: a list of K covariance matrices, each with dimensions 2x2
  sum( sapply(1:K, FUN=function(k) {
    p_s[k] * Sigma_s[[k]][1,2]^2 / (Sigma_s[[k]][1,1] * Sigma_s[[k]][2,2])
  }) )
}

# 1. initial K-lines clustering algorithm
klines_init <- function(x, y, K) {
  if (!requireNamespace("lmodel2", quietly = TRUE)) {
    stop("Package \"lmodel2\" needed for this function to work. Please install it.",
  	      call. = FALSE)
  }
  n <- length(x)
  membership <- sample(1:K, size=n, replace=TRUE) # randomly assign the n points to K lines
  while (length(unique(membership)) < K | any(table(membership) < 3)) {
    membership <- sample(1:K, size=n, replace=TRUE)
  }
  membership0 <- sample(membership) # initially set the "old" assignment as something different from the current assignment
  iter <- 0
  while (!identical(membership0, membership) & iter < 100) {
    # calculate the K lines (first principal components by major axis regression)
    lines <- sapply(1:K, FUN=function(i) {
      idx <- which(membership == i)
      if (var(y[idx]) > 0) {
        if (requireNamespace("lmodel2", quietly = TRUE)) {
          suppressMessages(reg_res <- lmodel2::lmodel2(y[idx]~x[idx])$regression.results)
        }
        a <- reg_res$Slope[reg_res$Method=="MA"]
        b <- reg_res$Intercept[reg_res$Method=="MA"]
      } else {
        a <- 0
        b <- y[idx][1]
      }
      return(c(a, b))
    })
    # calculate the perpendicular distance between each data point to the K lines
    perp_dist <- apply(lines, 2, FUN=function(beta) {
      a <- beta[1]
      b <- beta[2]
      abs(a*x - y + b) / sqrt(a^2 + 1)
    })
    # first let the current assignment be the "old" assignment
    membership0 <- membership
    # then update the current assignment by assigning every point to its closest line in terms of perpendicular distance
    membership <- max.col(-perp_dist, "first")
    while (length(unique(membership)) < K | any(table(membership) < 3)) {
      membership <- max.col(-perp_dist + matrix(rnorm(length(perp_dist), sd=sd(perp_dist)), nrow(perp_dist), ncol(perp_dist)), "first")
    }
    iter <- iter + 1
  }
  return(membership)
}

# 2. EM algorithm for initializing the K-lines clustering algorithm
EM <- function(x, y, K) {
  if (!requireNamespace("mvtnorm", quietly = TRUE)) {
    stop("Package \"mvtnorm\" needed for this function to work. Please install it.",
    	      call. = FALSE)
  }
  # a functoin to cluster a scatterplot to K lines by hard K-lines algorithm
  if (length(x) != length(y)) {
    return("ERROR: x and y do not have the same length")
  }
  # if (typeof(K) != "double") {
  # 	return("ERROR: K must be an integer")
  # } else if (K%%1 != 0) {
  # 	return("ERROR: K must be an integer")
  # }
  if (K%%1 != 0) {
    return("ERROR: K must be an integer")
  }
  n <- length(x)
  data <- cbind(x, y)
  membership <- klines_init(x, y, K) # use the K-lines clustering algorithm assign the n points to K lines
  # iter <- 0
  # while (length(unique(membership)) < K | any(table(membership) < 3) & iter < 100) {
  # 	membership <- klines_init(x, y, K)
  # 	iter <- iter + 1
  # }
  parameter <- lapply(1:K, FUN=function(i) {
    idx <- which(membership == i)
    p_k <- length(idx) / n
    data_k <- cbind(x[idx], y[idx])
    mu_k <- colMeans(data_k)
    Sigma_k <- cov(data_k)
    return(list(p_k, mu_k, Sigma_k))
  })
  parameter0 <- parameter
  diff <- 1 # initially set “diff” to be large
  iter <- 0
  while (diff > 10^(-3) & iter < 100) {
    # E-step
    joint_densities <- sapply(parameter, FUN=function(pam) {
      p_k <- pam[[1]]
      mu_k <- pam[[2]]
      Sigma_k <- pam[[3]]
      density_k <- mvtnorm::dmvnorm(data, mean = mu_k, sigma = Sigma_k)
      return(p_k * density_k)
    })
    kxi <- joint_densities / rowSums(joint_densities) # n x K
    kxi[is.na(kxi)] <- 0
    # first let the current parameter be the "old" parameter
    parameter0 <- parameter
    # then update the current parameter: M-step
    kxi_colsums <- colSums(kxi)
    p <- kxi_colsums / n # a K-dim vector
    mu <- t(kxi) %*% data / kxi_colsums # K x 2
    parameter <- lapply(1:K, FUN=function(i) {
      p_k <- p[i]
      mu_k <- mu[i,]
      sigma2_Xk <- kxi[,i] %*% (x - mu[i,1])^2 / kxi_colsums[i]
      sigma2_Yk <- kxi[,i] %*% (y - mu[i,2])^2 / kxi_colsums[i]
      sigma_XYk <- kxi[,i] %*% ((x - mu[i,1])*(y - mu[i,2])) / kxi_colsums[i]
      Sigma_k <- rbind( c(sigma2_Xk, sigma_XYk), c(sigma_XYk, sigma2_Yk) )
      return(list(p_k, mu_k, Sigma_k))
    })
    diff <- max(abs(unlist(parameter0) - unlist(parameter)))
    if (is.na(diff)) {
      diff <- 0
    }
    iter <- iter + 1
  }
  membership <- apply(kxi, 1, which.max)
  while (length(unique(membership)) < K | any(table(membership) < 3)) {
    membership <- max.col(-kxi + matrix(rnorm(length(kxi), sd=sd(kxi)), nrow(kxi), ncol(kxi)), "first")
  }
  return(membership)
}

# 3. K-lines clustering algorithm
klines_each <- function(x, y, K, ifchooseK=F) {
  if (!requireNamespace("lmodel2", quietly = TRUE)) {
    stop("Package \"lmodel2\" needed for this function to work. Please install it.",
	      call. = FALSE)
  }
  n <- length(x)
  # membership <- sample(1:K, size=n, replace=TRUE) # randomly assign the n points to K lines
  # while (length(unique(membership)) < K | any(table(membership) < 3)) {
  # 	membership <- sample(1:K, size=n, replace=TRUE)
  # }
  if (K==1) {
    membership <- rep(1, n)
  } else {
    membership <- EM(x, y, K)
  }

  # while (length(unique(membership)) < K | any(table(membership) < 3)) {
  # 	membership <- EM(x, y, K)
  # }
  membership0 <- sample(membership) # initially set the "old" assignment as something different from the current assignment
  iter <- 0
  while (!identical(membership0, membership) & iter < 100) {
    # calculate the K lines (first principal components by major axis regression)
    lines <- sapply(1:K, FUN=function(i) {
      idx <- which(membership == i)
      if (var(y[idx]) > 0) {
        suppressMessages(reg_res <- lmodel2::lmodel2(y[idx]~x[idx])$regression.results)
        a <- reg_res$Slope[reg_res$Method=="MA"]
        b <- reg_res$Intercept[reg_res$Method=="MA"]
      } else {
        a <- 0
        b <- y[idx][1]
      }
      return(c(a, b))
    })
    # calculate the perpendicular distance between each data point to the K lines
    perp_dist <- apply(lines, 2, FUN=function(beta) {
      a <- beta[1]
      b <- beta[2]
      abs(a*x - y + b) / sqrt(a^2 + 1)
    })
    # first let the current assignment be the "old" assignment
    membership0 <- membership
    # then update the current assignment by assigning every point to its closest line in terms of perpendicular distance
    membership <- max.col(-perp_dist, "first")
    while (length(unique(membership)) < K | any(table(membership) < 3)) {
      membership <- max.col(-perp_dist + matrix(rnorm(length(perp_dist), sd=sd(perp_dist)), nrow(perp_dist), ncol(perp_dist)), "first")
    }
    iter <- iter + 1
  }
  # calculate the incomplete-sample R2 measure
  R2_g <- sum( sapply(1:K, FUN=function(i) {
    idx <- which(membership == i)
    p_k <- length(idx) / n
    r2_k <- cor(x[idx], y[idx])^2
    p_k * r2_k
  }) )
  # calculate the perpendicular distance matrix (rows are data points, columns are lines)
  if (ifchooseK) {
    if (K==1) {
      if (var(y) > 0) {
        suppressMessages(reg_res <- lmodel2(y~x)$regression.results)
        a <- reg_res$Slope[reg_res$Method=="MA"]
        b <- reg_res$Intercept[reg_res$Method=="MA"]
      } else {
        a <- 0
        b <- y[1]
      }
      perp_dist <- matrix(abs(a*x - y + b) / sqrt(a^2 + 1), ncol=1)
    }
  }
  return( list(membership=membership, R2_g=R2_g, D=perp_dist) )
}

# the K-lines clustering algorithm function
Klines <- function(x, y, K, ifchooseK=F, num_init=1, mc.cores=1) {
  results <- parallel::mclapply(1:num_init, FUN=function(i) {
    klines_each(x, y, K, ifchooseK)
  }, mc.cores=mc.cores)
  idx <- which.max(sapply(results, FUN=function(x) x$R2_g))
  if (ifchooseK) {
    return(list(membership=results[[idx]]$membership, D=results[[idx]]$D))
  } else {
    return(membership=results[[idx]]$membership)
  }

}

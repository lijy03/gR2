# a function to calculate the supervised sample generalized R2: R^2_{G(S)}
R2gS <- function(x, y, z) {
  z_uniq <- unique(z)
  n <- length(z)
  result <- sapply(z_uniq, FUN=function(z_k) {
    idx <- which(z==z_k)
    p_k <- length(idx)/n
    bar_x_k <- mean(x[idx])
    bar_y_k <- mean(y[idx])
    x_s <- x[idx]
    y_s <- y[idx]
    r2_kc <- sum( (x_s - bar_x_k) * (y_s - bar_x_k) )^2 / ( sum( (x_s - bar_x_k)^2 ) * sum( (y_s - bar_y_k)^2 ) )
    return( c( p_k, r2_kc ) )
  })
  sum( result[1,] * result[2,] )
}

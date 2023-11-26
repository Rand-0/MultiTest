MCInt_t <- function(k, df, x_min, x_max = 100, n_random = 100000)
{
  if(any(x_min > x_max)) { return(0) }

  set.seed(1234567)

  if (length(x_min) == 1) x_min <- rep(x_min, k)

  w <- function(x, df)
  {
    resdt <- prod(dunif(x, x_min, x_max) / dt(x, df))
    resdt
  }

  X <- matrix(rt(n_random * k, df), ncol = k)

  Y <- apply(X, 1, function(row) mdt(row, df) * w(row, df))

  mean_Y <- mean(Y)
  result <- mean_Y * prod(x_max - x_min)
  result
}

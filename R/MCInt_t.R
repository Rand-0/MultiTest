MCInt_t <- function(k, df, x_min, x_max=100, n_random = 50000)
{
  w <- function(x, df)
  {
    resdt = 1
    for(i in 1:k)
    {
      resdt = resdt * dunif(x[i], x_min, x_max) / dt(x[i], df)
    }
    resdt
  }

  X = matrix(rt(n_random*k, df), ncol = k)

  Y = c()

  for(i in 1:n_random)
  {
    Y = rbind(Y,(mdt(X[i,], df)*w(X[i,], df)))
  }

  (mean(Y)*(x_max - x_min)^k)
}

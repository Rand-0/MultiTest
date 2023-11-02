MCInt_norm <- function(k, x_min, x_max=10, n_random = 50000)
{
  w <- function(x)
  {
    resdt = 1
    for(i in 1:k)
    {
      resdt = resdt * dunif(x[i], x_min, x_max) / dnorm(x[i])
    }
    resdt
  }

  X = matrix(rnorm(n_random*k), ncol = k)

  Y = c()

  for(i in 1:n_random)
  {
    Y = rbind(Y,(mdnorm(X[i,])*w(X[i,])))
  }

  (mean(Y)*(x_max - x_min)^k)
}

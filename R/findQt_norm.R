findQt_norm <- function(p, k)
{
  if(k == 1)
  {
    value = qnorm(p)
    error = 0
  } else
  {
    value = optimize(f = function(x)
      abs(cubature::adaptIntegrate(mdnorm, rep(x,k), rep(Inf, k))$integral - (1-p)),
      interval = c(-5, 5), tol = 1e-4)[1]
    value = as.numeric(value)
    error = cubature::adaptIntegrate(mdnorm, rep(value,k), rep(Inf, k))$error
  }
  c(value, error)
}

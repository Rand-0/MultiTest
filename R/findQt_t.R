findQt_t<- function(p, df, k)
{
  if(k == 1)
  {
    value = qt(p, df)
    error = 0
  } else
  {
    value = optimize(f = function(x)
      abs(cubature::adaptIntegrate(mdt, rep(x,k), rep(Inf, k), df)$integral - (1-p)),
             interval = c(-5, 5), tol = 1e-4)[1]
    value = as.numeric(value)
    error = cubature::adaptIntegrate(mdt, rep(value,k), rep(Inf, k), df)$error
  }

  c(value, error)
}

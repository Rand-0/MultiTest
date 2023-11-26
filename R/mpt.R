#' The multivariate Student t Distribution
#'
#' Returns CDF for the m-variate t distribution with 'df' degrees of freedom, for a vector of length 'm'.
#' @param X Vector of quantiles.
#' @param df degrees of freedom (>0).
#' @returns Numeric value.
#' @export
#' @examples
#' mpt(c(1,1), 100)
#' mpt(1)
mpt <- function(X, v = 1)
{
  k = length(X)

  if(k == 1)
  {
    value = (1 - pt(X, v))
    error = 0
  } else if(k == 2)
  {
    if(any(X >= 999)) { return(0) }

    bdt <- function(x, y) {1/(2*pi)*(1+(x^2+y^2)/v)^(-(v+2)/2)}

    value = pracma::integral2(bdt, X[1], 999, X[2], 999)$Q
    error = pracma::integral2(bdt, X[1], 999, X[2], 999)$error
  } else if(k == 3)
  {
    if(any(X >= 9)) { return(0) }

    tdt <- function(x, y, z)
    {
      ((v*pi)^(-3/2)*gamma(v/2+3/2)/gamma(v/2)*(1+(x^2+y^2+z^2)/v)^(-(v+3)/2))
    }

    value = pracma::integral3(tdt, X[1], 9, X[2], 9, X[3], 9)
    error = 0
  } else
  {
    value = cubature::adaptIntegrate(mdt, X, rep(9, k), v)$integral
    error = cubature::adaptIntegrate(mdt, X, rep(9, k), v)$error
  }
  c(value, error)
}

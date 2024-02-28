#' The multivariate Student t Distribution
#'
#' Returns density for the m-variate t distribution with 'df' degrees of freedom, for a vector of length 'm'.
#' @param X Vector of quantiles.
#' @param df degrees of freedom (>0).
#' @returns Numeric value.
#' @export
#' @examples
#' mdt(c(1,1), 100)
#' mdt(1)
mdt <- function(X, v = 1)
{
  k = length(X)
  c = (v*pi)^(-k/2)*exp(lgamma(v/2 + k/2) - lgamma(v/2))

  c*(1+(sum(X^2))/v)^(-(v+k)/2)
}

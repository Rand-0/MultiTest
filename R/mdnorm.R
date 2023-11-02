#' The multivariate Normal Distribution
#'
#' Returns density for the m-variate normal distribution for a vector of length 'm'.
#' @param X Vector of quantiles.
#' @returns Numeric value.
#' @examples
#' mdnorm(c(1,1))
#' mdnorm(1)
mdnorm <- function(X)
{
  k = length(X)
  c = 1/sqrt(2*pi^k)

  c*exp(-sum(X^2)/2)
}

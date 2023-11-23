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
mpt <- function(X, v = 1, tails)
{
  k = length(X)

  if(k == 1)
  {
    pt(X, v)
  } else if(k == 2)
  {
    bdt <- function(x, y) {1/(2*pi)*(1+(x^2+y^2)/v)^(-(v+2)/2)}

    pracma::integral2(bdt, -999, X[1], -999, X[2])$Q
  } else if(k == 3)
  {
    tdt <- function(x, y, z)
    {
      ((v*pi)^(-3/2)*gamma(v/2+3/2)/gamma(v/2)*(1+(x^2+y^2+z^2)/v)^(-(v+3)/2))
    }

    pracma::integral3(tdt, -9, X[1], -9, X[2], -9, X[3])
  } else
  {
    stop("CDF can only be computed for 3-dimensionals vectors or less!")
  }
}

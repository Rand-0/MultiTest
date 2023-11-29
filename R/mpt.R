#' The multivariate Student t Distribution
#'
#' Returns CDF for the m-variate t distribution with 'df' degrees of freedom, for a vector of length 'm'.
#' @param X Vector of quantiles.
#' @param df degrees of freedom (>0).
#' @param lower.tails logical vector; if FALSE (default), probablities P[X>x], otherwise, P[X≤x].
#' @returns Numeric value.
#' @export
#' @examples
#' mpt(c(1,1), 100)
#' mpt(1)
mpt <- function(X, v = 1, lower.tails = c(FALSE))
{
  k = length(X)

  if (length(lower.tails) == 1) { lower.tails = rep(lower.tails, k) }

  create_bounds <- function(x, lower.tail)
  {
    if(isTRUE(lower.tail))
    {
      if(x >= 9) { c(x, x + 1) } else { c(x, 9) }
    } else
    {
      if(x <= -9) { c(x - 1, x) } else { c(-9, x) }
    }
  }

  bounds = matrix(unlist(purrr::map2(X, lower.tails, create_bounds)), nrow = 2)

  if(k == 1)
  {
    value = pt(X, v, lower.tail = lower.tails)
    error = 0
  } else if(k == 2)
  {
    if(any(X >= 9)) { return(0) }

    bdt <- function(x, y) {1/(2*pi)*(1+(x^2+y^2)/v)^(-(v+2)/2)}

    value = pracma::integral2(bdt, bounds[1,1], bounds[2,1], bounds[1,2], bounds[2,2])$Q
    error = pracma::integral2(bdt, bounds[1,1], bounds[2,1], bounds[1,2], bounds[2,2])$error
  } else if(k == 3)
  {
    if(any(X >= 9)) { return(0) }

    tdt <- function(x, y, z)
    {
      ((v*pi)^(-3/2)*gamma(v/2+3/2)/gamma(v/2)*(1+(x^2+y^2+z^2)/v)^(-(v+3)/2))
    }

    value = pracma::integral3(tdt, bounds[1,1], bounds[2,1], bounds[1,2], bounds[2,2],
                              bounds[1,3], bounds[2,3])
    error = 0
  } else
  {
    value = cubature::adaptIntegrate(mdt, bounds[1,], bounds[2,], v)$integral
    error = cubature::adaptIntegrate(mdt, bounds[1,], bounds[2,], v)$error
  }
  c(value, error)
}

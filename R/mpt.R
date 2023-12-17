#' The multivariate Student t Distribution
#'
#' Returns CDF for the m-variate t distribution with 'df' degrees of freedom, for a vector of length 'm'.
#' @param X Vector of quantiles.
#' @param df degrees of freedom (>0).
#' @param lower.tails logical vector; if TRUE (default), probablities P[X≤x], otherwise, P[X>x].
#' @returns Numeric value.
#' @export
#' @examples
#' mpt(c(1,1), 100, c(TRUE,FALSE))
#' mpt(1)
mpt <- function(X, v = 1, lower.tails = c(TRUE))
{
  k = length(X)

  if (length(lower.tails) == 1 & k > 1) { lower.tails = rep(lower.tails, k) }

  create_bounds <- function(x, lower.tail)
  {
    if(!isTRUE(lower.tail)) { c(x, Inf) } else { c(-Inf, x) }
  }

  bounds = matrix(unlist(purrr::map2(X, lower.tails, create_bounds)), nrow = 2)

  result = unlist(cubature::adaptIntegrate(mdt, bounds[1,], bounds[2,], v)[1:2])

  names(result) = c("value", "error")
  result
}


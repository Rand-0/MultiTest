#' @title Student's t-Test for complex hypothesis for linear models.
#'
#' @description
#' Performs multivariate Student's t-Test on linear model.
#'
#'
#' @param object A fitted "lm" object or named vector of coefficients.
#' @param hypotheses Vector of null hypothesis.
#' @param alternatives Vector of alternative hypothesis, must be one of "not.equal", "greater" or "less".
#' @param alpha Float. Statistical significance, by default = 0.05.
#' @param vcov Covariance matrix. When supplied with "lm" object, it replaces the vcov matrix from model.
#' @param df Number of observation in model diminished by number of exogenous variables with constant term.
#' @param decision.criteria Character. Which decision criterion should be displayed when printing result, either "p-value", "critical.region" or "both" (default).
#' @param merge. Logical. When FALSE is supplied, identical elements of hypothesis will not be merged.
#'
#' @return A list with class "Clhtest" containing the following components:
#'
#' @export
#' @family CLH
#' @examples
#' x <- runif(100, min = 0, max = 100)
#' z <- runif(100, min = 0, max = 100)
#' q <- runif(100, min = 0, max = 100)
#' e <- rnorm(100, mean = 0, sd = 1)
#' y <- 4*x + 5*z + 6*q + 1 + e
#' data <- data.frame(x, z, q, y)
#' model <- lm(y ~ x + z + q, data)
#' Cl_test_t(model, c("x=4", "z=5", "q=6"), c("not.equal", "less", "greater"))
Cl_test_t <- function(object, hypotheses, alternatives, alpha = 0.05, vcov = NULL, df = NULL,
                      decision.criteria = "both", merge. = NULL)
{
  UseMethod("Cl_test_t")
}

#' @rdname Cl_test_t
#' @export
Cl_test_t.lm <- function(object, hypotheses, alternatives, alpha = 0.05, vcov = NULL,
                         decision.criteria = "both", merge. = NULL)
{
  if(is.null(vcov)) { vcov = vcov(object) }

  data.name = deparse(substitute(object))

  ComplexLinearHypothesis(object$coefficients, vcov, object$df.residual,
                          hypotheses, alternatives, alpha, decision.criteria,
                          merge., "mvt", data.name = data.name)
}

#' @rdname Cl_test_t
#' @export
Cl_test_t.default <- function(object, hypotheses, alternatives, alpha = 0.05, vcov, df,
                              decision.criteria = "both", merge. = NULL)
{
  ComplexLinearHypothesis(object, vcov, df,
                          hypotheses, alternatives, alpha, decision.criteria,
                          merge., "mvt")
}

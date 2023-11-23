#' Student's t-Test with multiple hypothesis for linear regression model
#'
#' @param object A fitted "lm" object.
#' @param hypotheses Vector of null hypothesis.
#' @param alternatives Vector of alternative hypothesis, must be one of "two.sided", "greater" or "less".
#' @param alpha Float. Statistical significance, by default = 0.05.
#'
#' @return An object containg list of hypothesis, value of t-test statistics and corresponding critical regions.
#' @export
#'
#' @examples
#' x <- runif(100, min = 0, max = 100)
#' z <- runif(100, min = 0, max = 100)
#' q <- runif(100, min = 0, max = 100)
#' e <- rnorm(100, mean = 0, sd = 1)
#' y <- 4*x + 5*z + 6*q + 1 + e
#' data <- data.frame(x, z, q, y)
#' model <- lm(y ~ x + z + q, data)
#' t_test_combo.lm(model, c("x=4", "z=5", "q=6"), c("not.equal", "less", "greater"))
t_test_combo.lm <- function(object, hypotheses, alternatives, alpha = 0.05)
{
  Hs_0 = sapply(hypotheses, stringr::str_split_1, "=")
  Hs_1 = alternatives

  H_count = length(Hs_1)
  df = object$df.residual

  if ((length(Hs_0)/2) != length(Hs_1))
  {stop("List of hypotheses and alternatives should be equal!")}

  T_stats = c()
  T_interval = c()

  Qs_t = c(findQt(1-alpha, df, H_count, "t"), findQt(1-alpha/(2^H_count), df, H_count, "t"))

  Qs_t = c(as.numeric(Qs_t[1]), as.numeric(Qs_t[2]))

  for(i in 1:H_count)
  {
    T_i = (object$coefficients[Hs_0[1,i]] - as.numeric(Hs_0[2,i])) / sqrt(diag(vcov(object)))[Hs_0[1,i]]
    T_stats = append(T_stats, round(T_i,4))

    if (Hs_1[i] == "not.equal")
    {
      T_interval_i = c(-round(Qs_t[2],4), round(Qs_t[2],4))
      T_interval = rbind(T_interval, T_interval_i)
    } else if(Hs_1[i] == "greater")
    {
      T_interval_i = c(round(Qs_t[2],4), Inf)
      T_interval = rbind(T_interval, T_interval_i)
    } else if(Hs_1[i] == "less")
    {
      T_interval_i = c(-Inf, -round(Qs_t[2],4))
      T_interval = rbind(T_interval, T_interval_i)
    }
  }

  if(H_count <= 3)
  {
    P_value = mpt(replace(T_stats, T_stats>0, -T_stats), df)
  } else
  {
    P_value = "P-value cannot be computed for more than 3 hypotheses!"
  }

  result = list(statistcs = T_stats, parameter = df, p.value = P_value,
                conf.int = T_interval, estimate = object$coefficients[Hs_0[1,]],
                null.value = as.numeric(Hs_0[2,]), stderr = sqrt(diag(vcov(object)))[Hs_0[1,]],
                alternative = alternatives, method = "Muiltivariate t-test",
                data.name = deparse(substitute(object)))

  result
}

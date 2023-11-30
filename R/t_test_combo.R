#' Student's t-Test with multiple hypothesis for linear regression model
#'
#' @param object A fitted "lm" object.
#' @param hypotheses Vector of null hypothesis.
#' @param alternatives Vector of alternative hypothesis, must be one of "not.equal", "greater" or "less".
#' @param alpha Float. Statistical significance, by default = 0.05.
#' @param vcov Covariance matrix. When supplied with "lm" object, it replaces the vcov matrix from model.
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
#' t_test_combo(model, c("x=4", "z=5", "q=6"), c("not.equal", "less", "greater"))
t_test_combo <- function(object, hypotheses, alternatives, alpha = 0.05, vcov = NULL, f_debug = 0)
{
  UseMethod("t_test_combo")
}

#' @rdname t_test_combo
#' @export
t_test_combo.lm <- function(object, hypotheses, alternatives, alpha = 0.05, vcov = NULL, f_debug = 0)
{
  startTime = Sys.time()

  Hs_0 = sapply(hypotheses, stringr::str_split_1, "=")
  Hs_1 = alternatives

  H_count = length(Hs_1)
  df = object$df.residual

  if(is.null(vcov)) { vcov = sqrt(diag(vcov(object)))
  } else {vcov = sqrt(diag(vcov))}

  if(H_count > 5) {stop("Method does not support more than 5 hypotheses!")}

  if ((length(Hs_0)/2) != length(Hs_1))
  {stop("List of hypotheses and alternatives should be equal!")}

  T_stats = c()
  T_interval = c()
  T_interval_i = c()
  Tsides_alt = 0
  T_stats_pval = c()
  Tsides_pval = c()

  Qs_t_1 = findQt(1-alpha, df, H_count, "t")
  Qs_t_2 = findQt(1-alpha/(2^H_count), df, H_count, "t")

  Qs_t_1 = c(as.numeric(Qs_t_1[1]), as.numeric(Qs_t_1[2]))
  Qs_t_2 = c(as.numeric(Qs_t_2[1]), as.numeric(Qs_t_2[2]))

  for(i in 1:H_count)
  {
    T_i = (object$coefficients[Hs_0[1,i]] - as.numeric(Hs_0[2,i])) / vcov[Hs_0[1,i]]
    T_stats = append(T_stats, round(T_i,4))

    if(f_debug == 1) {print("Statystyki: ", T_i, "\n")}

    if (Hs_1[i] == "not.equal")
    {
      T_interval_i = c(-Inf, -round(Qs_t_2[1],4), round(Qs_t_2[1],4), Inf)
      T_interval = rbind(T_interval, T_interval_i)

      Tsides_alt = Tsides_alt  + 1
      T_stats_pval = append(T_stats_pval, abs(round(T_i,4)))
      Tsides_pval = append(Tsides_pval, TRUE)

    } else if(Hs_1[i] == "greater")
    {
      T_interval_i = c(NA, NA, round(Qs_t_1[1],4), Inf)
      T_interval = rbind(T_interval, T_interval_i)
      T_stats_pval = append(T_stats_pval, round(T_i,4))
      Tsides_pval = append(Tsides_pval, TRUE)
    } else if(Hs_1[i] == "less")
    {
      T_interval_i = c(-Inf, -round(Qs_t_1[1],4), NA, NA)
      T_interval = rbind(T_interval, T_interval_i)
      T_stats_pval = append(T_stats_pval, round(T_i,4))
      Tsides_pval = append(Tsides_pval, FALSE)
    }
  }

  P_value = signif(2^Tsides_alt*mpt(T_stats_pval, df, Tsides_pval),4)

  if(any(P_value < 1e-10)) { P_value = c(-1,-1)}

  names(P_value) = c("estimate", "error")

  dimnames(T_interval)[[1]] = unname(Hs_0[1,])
  dimnames(T_interval)[[2]] = c("n.bound.l","n.bound.u","p.bound.l","p.bound.u")

  names(df) = "df"

  errors = c(Qs_t_1[2], Qs_t_2[2])

  names(errors) = c("one.sided", "two.sided")

  if(Tsides_alt == 0) {errors = errors[1]}

  endTime = Sys.time()

  result = list(statistics = T_stats, parameter = df, p.value = P_value,
                critical.area = T_interval, quant.err = errors,
                estimate = object$coefficients[Hs_0[1,]], null.value = as.numeric(Hs_0[2,]),
                std.err = sqrt(diag(vcov(object)))[Hs_0[1,]],
                alternative = alternatives, method = "Muiltivariate t-test",
                data.name = deparse(substitute(object)), exec.time = endTime - startTime)

  class(result) = "MultiTest"

  result
}

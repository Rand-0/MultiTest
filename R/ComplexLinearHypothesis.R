#' @title Test Complex Linear Hypothesis
#'
#' @description
#' Generic function for testing a complex linear hypothesis, including multivariate
#' Student's t-Test for generalized linear models (OLS and MM regressions) and
#' z-Test for MLE parameters.
#'
#' @family CLH
#'
#' @param coefficients Named vector of coefficients.
#' @param vcov Covariance matrix.
#' @param df Degrees of freedom.
#' @param hypotheses Vector of null hypothesis.
#' @param alternatives Vector of alternative hypothesis, must be one of "not.equal", "greater" or "less".
#' @param alpha Float. Statistical significance, by default = 0.05.
#' @param decision.criteria Character. Which decision criterion should be displayed when printing result, either "p-value" (experimental), "critical.region" (default) or "both."
#' @param merge. Logical. When FALSE is supplied, identical elements of hypothesis will not be merged.
#' @param family Character. Type of test which should be performed, either "mvt" (multivariate t-Test) or "mvnorm" (multivariate z-Test).
#' @param ... Arguments to pass down.
#'
#' @export
ComplexLinearHypothesis <- function(coefficients, vcov, df,
                                    hypotheses, alternatives, alpha = 0.05,
                                    decision.criteria = "critical.region", merge. = NULL, family, ...)
{
    family_check = list(family = family)
    class(family_check) = "mt_family"
    validateObject(family_check)

  data.name = ifelse(!is.null(list(...)["data.name"]), list(...)["data.name"], NULL)

  if(family == "mvt")
  {
    ComplexLinearHypothesis.mvt(coefficients, vcov, df,
                                hypotheses, alternatives, alpha,
                                decision.criteria, merge., data.name)
  } else if(family == "mvnorm")
  {
    ComplexLinearHypothesis.mvnorm(coefficients, vcov, df,
                                   hypotheses, alternatives, alpha,
                                   decision.criteria, merge., data.name)
  }
}


#' @description
#' CLH is an alias for ComplexLinearHypothesis.
#'
#' @export
#' @rdname ComplexLinearHypothesis
CLH <- function(coefficients, vcov, df,
                hypotheses, alternatives, alpha = 0.05,
                decision.criteria = "critical.region", merge. = NULL, family, ...)
{
  ComplexLinearHypothesis(coefficients, vcov, df,
                          hypotheses, alternatives, alpha,
                          decision.criteria, merge., family, ...)
}

#' @rdname ComplexLinearHypothesis
ComplexLinearHypothesis.mvt <- function(coefficients, vcov, df,
                                        hypotheses, alternatives, alpha = 0.05,
                                        decision.criteria = "critical.region", merge. = NULL, data.name)
{
  options(warn = 1)

  startTime = Sys.time()

    hs_check = hypotheses
    class(hs_check) = "mt_hs"
    validateObject(hs_check, alternatives, merge.)

    decision_check = decision.criteria
    class(decision_check) = "mt_decision"
    validateObject(decision_check)

  Hs_0 = sapply(hypotheses, stringr::str_split_1, "=")
  Hs_1 = alternatives

  if(ifelse(is.null(merge.), TRUE, merge.))
  {
    cHs_0 = c()
    cHs_1 = c()
    for(i in 1:length(Hs_1))
    {
      if(Hs_0[1,i] != "")
      {
        cHs_0 = append(cHs_0, Hs_0[1:2,i])
        cHs_1 = append(cHs_1, Hs_1[i])
      }
      if(Hs_0[1,i] %in% Hs_0[1,-i])
      {
        Hs_0[1,] = replace(Hs_0[1,], Hs_0[1,] == Hs_0[1,i], "")
      }
    }
    Hs_0 = matrix(cHs_0, nrow = 2)
    Hs_1 = cHs_1
  }

  H_count = length(Hs_1)
  coeff = coefficients

    coeff_check = coeff
    class(coeff_check) = "mt_coeff"
    validateObject(coeff_check, Hs_0)

    vcov_check = vcov
    class(vcov_check) = "mt_vcov"
    validateObject(vcov_check, Hs_0)

    params_check = c(df, alpha)
    class(params_check) = "mt_params"
    validateObject(params_check)

  vcov = sqrt(diag(vcov))

  vcov = replace(vcov, vcov == 0, 1e-10)

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
    T_i = (coeff[Hs_0[1,i]] - as.numeric(Hs_0[2,i])) / vcov[Hs_0[1,i]]
    T_stats = append(T_stats, round(T_i,4))

    if (Hs_1[i] == "not.equal")
    {
      T_interval_i = c(-Inf, -round(Qs_t_2[1],4), round(Qs_t_2[1],4), Inf)
      T_interval = rbind(T_interval, T_interval_i)

      Tsides_alt = Tsides_alt  + 1
      T_stats_pval = append(T_stats_pval, abs(round(T_i,4)))
      Tsides_pval = append(Tsides_pval, FALSE)

    } else if(Hs_1[i] == "greater")
    {
      T_interval_i = c(NA, NA, round(Qs_t_1[1],4), Inf)
      T_interval = rbind(T_interval, T_interval_i)
      T_stats_pval = append(T_stats_pval, round(T_i,4))
      Tsides_pval = append(Tsides_pval, FALSE)

    } else if(Hs_1[i] == "less")
    {
      T_interval_i = c(-Inf, -round(Qs_t_1[1],4), NA, NA)
      T_interval = rbind(T_interval, T_interval_i)
      T_stats_pval = append(T_stats_pval, round(T_i,4))
      Tsides_pval = append(Tsides_pval, TRUE)

    }
  }

  P_value = signif(2^Tsides_alt*mpt(T_stats_pval, df, Tsides_pval), 4)

  #P-value correction for mv tests
  T_stats_pval = corPval(T_stats_pval, Hs_1, Tsides_pval)

  corP_value = signif(2^Tsides_alt*mpt(T_stats_pval, df, Tsides_pval), 4)

  if(P_value[1] == 0)
  {
    EQI = round(((P_value[1] - corP_value[1])/1e-10), 2)
  } else
  {
    EQI = round(((P_value[1] - corP_value[1])/P_value[1]), 2)
  }

    EQI_check = EQI
    class(EQI_check) = "mt_EQI"
    validateObject(EQI_check)

  P_value = corP_value

  P_value[(P_value < 1e-8 & P_value > 0)] = 1e-10

  #TODO: dont do it like this, it's embarrassing
  P_value[P_value < 0] = 0
  P_value[P_value > 1] = 1

  names(P_value) = c("estimate", "error")

  PI = calcPI(T_stats, Hs_1, Qs_t_1[1], Qs_t_2[1])

  T_interval = cbind(T_interval, PI)

  dimnames(T_interval)[[1]] = unname(Hs_0[1,])
  dimnames(T_interval)[[2]] = c("n.bound.l","n.bound.u","p.bound.l","p.bound.u", "PI")

  params = c(df, alpha, EQI)

  names(params) = c("df", "alpha", "EQI")

  errors = c(Qs_t_1[2], Qs_t_2[2])

  names(errors) = c("one.sided", "two.sided")

  if(Tsides_alt == 0) {errors = errors[1]}

  endTime = Sys.time()

  result = list(statistics = T_stats, parameters = params, p.value = P_value,
                critical.area = T_interval, quant.err = errors,
                estimate = coeff[Hs_0[1,]], null.value = as.numeric(Hs_0[2,]),
                std.err = sqrt(diag(vcov))[Hs_0[1,]],
                alternative = Hs_1, method = "Multivariate t-test",
                data.name = unlist(data.name), exec.time = endTime - startTime)

  class(result) = "Clhtest"

  attr(result, 'print.method') = switch(decision.criteria,
                                        "p-value" = 0,
                                        "critical.region" = 1,
                                        "both" = 2)
  options(warn = 0)

  result
}

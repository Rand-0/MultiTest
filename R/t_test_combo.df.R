#' Student's t-Test with multiple hypothesis
#'
#' @param object A data frame.
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
#' data <- data.frame(x, z, q)
#' t_test_combo.df(data, c("x=4", "z=5", "q=6"), c("not.equal", "less", "greater"))
t_test_combo.df <- function(object, hypotheses, alternatives, alpha = 0.05)
{
  Hs_0 = sapply(hypotheses, stringr::str_split_1, "=")
  Hs_1 = alternatives

  H_count = length(Hs_1)
  df = nrow(object)

  if ((length(Hs_0)/2) != length(Hs_1))
  {stop("List of hypotheses and alternatives should be equal!")}

  T_stats = c()
  T_interval = c()

  Qs_t = c(findQt(1-alpha, df, H_count, "t"), findQt(1-alpha/(2^H_count), df, H_count, "t"))

  Qs_t = c(as.numeric(Qs_t[1]), as.numeric(Qs_t[2]))

  for(i in 1:H_count)
  {
    T_mean = mean(object[,Hs_0[1,i]])
    t_sd = sd(object[,Hs_0[1,i]])
    T_i = (T_mean - as.numeric(Hs_0[2,i])) / t_sd
    T_stats = append(T_stats, round(T_i,4))

    if (Hs_1[i] == "not.equal")
    {
      T_interval_i = paste0("(-Inf ; -", round(Qs_t[2],4), ")u(", round(Qs_t[2],4), " ; +Inf)")
      T_interval = append(T_interval, T_interval_i)

    } else if(Hs_1[i] == "greater")
    {
      T_interval_i = paste0("(", round(Qs_t[1],4), " ; +Inf)")
      T_interval = append(T_interval, T_interval_i)

    } else if(Hs_1[i] == "less")
    {
      T_interval_i = paste0("(-Inf ; -", round(Qs_t[1],4), ")")
      T_interval = append(T_interval, T_interval_i)
    }
  }

  result = cbind(T_stats, T_interval)
  colnames(result) = c("t-test statistics", "Critical region")

  if(H_count <= 3)
  {
    P_value = mpt(replace(T_stats, T_stats>0, -T_stats), df)
  }

  cat(result)
  cat("------------------------------")
  cat("P-value = ", P_value)

}

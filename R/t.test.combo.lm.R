t.test.combo.lm <- function(model, hypotheses, alternatives, alpha = 0.05)
{
  Hs_0 = sapply(hypotheses, stringr::str_split_1, "=")
  Hs_1 = alternatives

  H_count = length(Hs_1)
  df = model$df.residual

  if ((length(Hs_0)/2) != length(Hs_1))
  {stop("List of hypotheses and alternatives should be equal!")}

  T_stats = c()
  T_interval = c()

  Qs_t = c(findQt(1-alpha, df, H_count, "t"), findQt(1-alpha/(2^H_count), df, H_count, "t"))

  Qs_t = c(as.numeric(Qs_t[1]), as.numeric(Qs_t[2]))

  for(i in 1:H_count)
  {
    T_i = (model$coefficients[Hs_0[1,i]] - as.numeric(Hs_0[2,i])) / sqrt(diag(vcov(model)))[Hs_0[1,i]]
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

  result
}

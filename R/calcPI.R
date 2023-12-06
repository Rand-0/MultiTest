calcPI <- function(T_stats, alt, qT_1, qT_2)
{
  result =  c()
  for(i in 1:length(T_stats))
  {
    qT_point = sapply(alt, function(x) switch(x,
                                              "less" = -qT_1,
                                              "greater" = qT_1,
                                              "not.equal" = qT_2))
    consN = dist(rbind(qT_point, T_stats))
    if(alt[i] == "less")
    {
      result = append(result, (qT_1 + T_stats[i])/consN)
    } else if(alt[i] == "greater")
    {
      result = append(result, (qT_1 - T_stats[i])/consN)
    } else
    {
      result = append(result, (qT_2 - abs(T_stats[i]))/consN)
    }
  }
  round(result, 4)
}

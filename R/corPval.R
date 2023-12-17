corPval <- function(bounds, pval, v, Tsides_alt)
{
  bounds = t(bounds)

  P_value = unlist(cubature::adaptIntegrate(mdt, bounds[1,], bounds[2,], v)[1:2])

  pval = pval + P_value

  pval = signif(2^Tsides_alt*pval,4)

  c(pval, P_value[1])
}

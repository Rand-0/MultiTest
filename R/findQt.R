findQt <- function(p, df = NULL, k, distribution)
{
  if (distribution == "t")
  {
    if (is.null(df)) {stop("This distribution requires specifying degrees of freedom (df)!")}

    findQt_t(p, df, k)
  }
  else if (distribution == "norm")
  {
    findQt_norm(p, k)
  }
}

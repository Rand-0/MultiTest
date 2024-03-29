corPval <- function(t_stats, alt, sides_pval)
{
  find_point <- function(t, a)
  {
    left = t[which(a == "less")]
    right = t[which(a != "less")]

    if(length(left) == 0 & !is.null(length)) { p.max = NULL } else { p.max = max(left)}
    if(length(right) == 0 & !is.null(right)) { p.min = NULL } else { p.min = min(right)}

    if(is.null(p.max)) { p.min } else if(is.null(p.min)) { p.max } else
    {
      p.p = which.max(c(p.max, -p.min))
      ifelse(p.p == 1, p.max, p.min)
    }
  }

  p.p_val = find_point(t_stats, alt)
  side = ifelse(alt[which(t_stats == p.p_val)[1]] == "less", TRUE, FALSE)

  unname(sapply(sides_pval, function(x) if(x == side) { p.p_val } else { -p.p_val }))
}

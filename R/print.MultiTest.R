#' @rdname print
#' @export
print.MultiTest <- function(object)
{
  concate_hypothesis <- function(param, x, val)
  {
    if(x == "not.equal")
    {
      paste("Beta_", param, " is not equal to ", val, "\n", sep = "")
    } else if(x == "less")
    {
      paste("Beta_", param, " is less than ", val, "\n", sep = "")
    } else if(x == "greater")
    {
      paste("Beta_", param, " is greater than ", val, "\n", sep = "")
    }
  }

  show_pval <- function(pval)
  {
    if (pval <= 0.001) { paste(pval, "***")
    } else if (pval <= 0.01) { paste(pval, "**")
    } else if (pval <= 0.05) { paste(pval, "*")
    } else if (pval <= 0.1) { paste(pval, ".")
    } else { paste (pval)}
  }

  cat("\n            ", object$method, "\n\n")
  cat("data: ", object$data.name, "\n")
  cat("dim = ", length(object$statistics), ", df = ", object$parameter, sep = "")
  cat(", t = (", paste(object$statistics, collapse = ", "), ")\n", sep = "")
  cat("p-value = ", object$p.value[1], #show_pval(object$p.value[1])
      " (error = ", object$p.value[2], ")\n", sep = "")
  #cat(rep("-", 3), "\n", sep = "")
  #cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n")

  cat("\nalternative hypothesis:\n")
  cat(unlist(purrr::pmap(list(names(object$statistics), object$alternative, object$null.value),
                  concate_hypothesis)), sep = "")
  cat("\n", rep("-", 3), "\n", sep = "")
  cat("Execution time: ", as.numeric(object$exec.time), " ",
      attr(object$exec.time, "units"), sep = "")
}

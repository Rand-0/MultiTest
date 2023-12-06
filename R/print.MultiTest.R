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

  convert_pval <- function()
  {
    if(all(object$p.value == -1))
    {
      object$p.value = c(1e-10, 1e-10)
      cat("p-value < ", object$p.value[1], " (error < ", object$p.value[2], ")\n", sep = "")
    } else if (object$p.value[1] == -1)
    {
      object$p.value[1] = 1e-10
      cat("p-value < ", object$p.value[1], " (error = ", object$p.value[2], ")\n", sep = "")
    } else if (object$p.value[2] == -1)
    {
      object$p.value[2] = 1e-10
      cat("p-value = ", object$p.value[1], " (error < ", object$p.value[2], ")\n", sep = "")
    } else
    {
      cat("p-value = ", object$p.value[1], " (error = ", object$p.value[2], ")\n", sep = "")
    }
  }

  showCriteria <- function(type)
  {
    if(type == 0) { convert_pval() } else if (type == 1)
    {
      cat("\n")
      print(object$critical.area)
    } else
    {
      convert_pval()
      cat("\n")
      print(object$critical.area)
    }
  }

  cat("\n            ", object$method, "\n\n")
  cat("data: ", object$data.name, "\n")
  cat("dim = ", length(object$statistics), ", df = ", object$parameters[1],
      ", alpha = ", object$parameters[2], sep = "")
  cat(", t = (", paste(object$statistics, collapse = ", "), ")\n", sep = "")
  showCriteria(attr(object, 'print.method'))

  #cat(rep("-", 3), "\n", sep = "")
  #cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n")

  cat("\nalternative hypothesis:\n")
  cat(unlist(purrr::pmap(list(names(object$statistics), object$alternative, object$null.value),
                  concate_hypothesis)), sep = "")
  cat("\n", rep("-", 3), "\n", sep = "")
  cat("Execution time: ", as.numeric(object$exec.time), " ",
      attr(object$exec.time, "units"), sep = "")
}

validateObject <- function(object, params = NULL, params2 = NULL)
{
  UseMethod("validateObject")
}

#' @export
validateObject.mt_coeff <- function(object, params)
{
  n_params = params[1,]

  if(all(n_params %in% names(object))) { return(invisible(NULL)) } else {
    stop("At least one of given coefficients does not appear in the model!",
         call. = FALSE)
  }
}

#' @export
validateObject.mt_vcov <- function(object, params)
{
  if(!is.matrix(object) | !is.numeric(object))
  {stop("Covariance matrix should be numeric matrix!", call. = FALSE)}

  n_params = params[1,]

  if(!all(colnames(object) %in% rownames(object)))
  {stop("Given covariance matrix is not a covariance matrix of the model parameters!", call. = FALSE)}

  if(!all(n_params %in% colnames(object)))
  {
    stop("In the covariance matrix at least one of given coefficients does not exist!",
         call. = FALSE)
  }

  if(any(diag(object) == 0))
  {
    warning("At least one variance of given parameters = 0. Estimations will be biased.\n",
            call. = FALSE)
  }
}

#' @export
validateObject.mt_hs <- function(object, params, params2)
{
  if(ifelse(is.null(params2), FALSE, params2) != FALSE)
  {stop("Incorrect parameter merge. - use only FALSE, or default NULL!", call. = FALSE)}

  if(!is.character(object))
  {stop("Null hypothesis should be character type!", call. = FALSE)}

  object = sapply(object, stringr::str_split_1, "=")

  if(is.list(object) | is.vector(object))
  {stop('Incorrect structure of null hypothesis. Should follow "NameOfParam=Value" structure!'
        , call. = FALSE)}

  if(is.array(object) & (nrow(object) > 2))
  {stop('Incorrect structure of null hypothesis. Should follow "NameOfParam=Value" structure!'
        , call. = FALSE)}

  if(!is.character(params) | !is.vector(params))
  {stop("Alternative hypothesis should be character type (or character vector)!", call. = FALSE)}

  if ((length(object)/2) != length(params))
  {stop("List of hypotheses and alternatives should be equal!", call. = FALSE)}

  if(length(params) > 5) {stop("Method does not support more than 5 hypotheses!",
                             call. = FALSE)}

  if(!all(params %in% c("less", "not.equal", "greater")))
  {stop('Incorrect structure of alternative hypothesis. Should contain only "less", "not.equal" or "greater"!',
          call. = FALSE)}

  mes = FALSE

  for(i in 1:length(params))
  {
    p = object[1,i]
    object[1,i] = ""
    index = match(p, object[1,], 0)
    if(index != 0)
    {
      if(object[2,i] != object[2,index[1]])
      {
        er = paste("Parameter for variable", p,
                   "has been specified >2 times, with different values!")
        stop(er, call. = FALSE)
      } else if(params[i] == params[index] & is.null(params2))
      {
        warning("Identical parts of hypothesis has been merged into one, to avoid unintentional results.",
                call. = FALSE)
        warning('If you aware of interpretation in this case, use "merge. = FALSE" in function call.',
                call. = FALSE)
      } else if (params[i] == params[index] & !ifelse(is.null(params2), TRUE, params2) & !mes)
      {
        mes = TRUE
        message('With given parameter "merge. = FALSE", identical parts of hypothesis has not been merged.',
                appendLF = TRUE)
      }
    }
  }
}

#' @export
validateObject.mt_params <- function(object, params)
{
  if(!is.numeric(object[1]))
    {stop("Degrees of freedom and parameter alpha should be numeric type!", call. = FALSE)}

  if(object[1] < 2) {stop("Method supports models with >2 degrees of freedom", call. = FALSE)
  } else if(object[1] < 50)
    {warning("Insufficent number of observation in model. Results might be biased.\n", call. = FALSE)}

  if(object[2] <= 0 | object[2] >= 1)
    {stop("Parameter alpha exceeds the range (0;1)!", call. = FALSE)}
}

#' @export
validateObject.mt_decision <- function(object)
{
  if(length(object) != 1)
  {stop('Incorrect decision criteria. Should be one of "p-value", "critical.region" or "both"!',
        call. = FALSE)}

  if(!(object %in% c("p-value", "critical.region", "both")))
  {stop('Incorrect decision criteria. Should be one of "p-value", "critical.region" or "both"!',
          call. = FALSE)}
}

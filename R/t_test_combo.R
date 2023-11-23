t_test_combo <- function(object, ...)
{
  setClass("lm_object", contains = "lm")

  setClass("data_frame", contains = "data.frame")

  class_input <- class(object)

  if (class_input == "lm_object")
  {
    method_name <- "lm_object"
  } else if (class_input == "data_frame")
  {
    method_name <- "data_frame"
  } else
  {
    stop("Invalid input class. Supported classes are 'lm_object' and 'data_frame'.")
  }

  method <- paste("t_test_combo", method_name, sep = ".")

  if (exists(method, mode = "function"))
  {
    result <- do.call(method, list(object, ...))
  } else
  {
    stop("Method not found for the provided input class.")
  }

  return(result)
}

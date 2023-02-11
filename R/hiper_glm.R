#' GLM function for high-dimensional data
#'
#' @param design the design matrix
#' @param outcome the outcome vector/matrix of the model
#' @param model choose the type of GLM
#'
#' @return S3 object of the model fitting output
#'
#' @export
#'
hiper_glm <- function(desgin, outcome, model = "linear"){
  # init_coef can be an optional argument for more advanced users
  supported_model <- c("linear","logit")
  if (!model %in% supported_model){
    stop(sprintf("The model %s is not supported!", model))
  }
  # TODO: find MLE
  hglm_out <- list()
  class(hglm_out) <- "hglm" # turn it into an S3 object
  warning("hglm out is not yet implemented.")
  return(hglm_out)
}

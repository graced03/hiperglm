#' Coefficients of the GLM model
#'
#' @param hglm_out, output of the GLM model
#'
#' @return coefficient vector
#'
#' @export
#'
coef.hglm <- function(hglm_out){
  warning("To be implemented.")
}

#' Variance-covariance matrix of the GLM model
#'
#' @param hglm_out, output of the GLM model
#'
#' @return variance-covariance matrix of the GLM model
#'
#' @export
#'
vcov.hglm <- function(hglm_out){
  warning("To be implemented.")
}

#' Print the GLM model fitting results
#'
#' @param hglm_out, output of the GLM model
#'
#' @return summary statistics of the GLM model
#'
#' @export
#'
print.hglm <- function(hglm_out){
  cat("Output of hiper_glm.")
}

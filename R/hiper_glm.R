#' GLM function for high-dimensional data
#'
#' @param X the design matrix
#' @param y the outcome vector/matrix of the model
#' @param model choose the type of GLM
#'
#' @return S3 object of the model fitting output
#'
#' @export
#'
hiper_glm <- function(X, y, model = "linear", option = list(mle_solver = 'pseduo-inverse')){
  # init_coef can be an optional argument for more advanced users
  supported_model <- c("linear","logit")
  if (!model %in% supported_model){
    stop(sprintf("The model %s is not supported!", model))
  }
  # TODO: find MLE
  hglm_out <- list()
  class(hglm_out) <- "hglm" # turn it into an S3 object

  if (model == "linear"){
    if (option$mle_solver == "pseduo-inverse") {
      hglm_out$coef_est <- lm_ols(X, y)# X^T y = X^T X \beta
      hglm_out$mle_solver <- "OLS"
    }
    else if (option$mle_solver == "BFGS") {
      hglm_out$coef_est <- lm_bfgs(X, y)
      hglm_out$mle_solver <- "BGFS"
    }
  }
  warning("Only linear regression has been implemented")
  return(hglm_out)
}



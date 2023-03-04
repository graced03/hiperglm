#' GLM function for high-dimensional data
#'
#' @param X the design matrix
#' @param y the outcome vector/matrix of the model
#' @param model choose the type of GLM
#' @param option optional settings for the model, including the choice of MLE solver
#'
#' @return S3 object of the model fitting output
#'
#' @export
#'
hiper_glm <- function(X, y, model = "linear", option = list(mle_solver="OLS")){
  supported_model <- c("linear","logit")
  if (!model %in% supported_model){
    stop(sprintf("The model %s is not supported!", model))
  }
    hglm_out <- list()
  class(hglm_out) <- "hglm"

  if (model == "linear"){
    if (option$mle_solver == "OLS") {
      hglm_out$coef <- lm_ols(X, y)
      hglm_out$mle_solver <- "OLS"
    }
    else if (option$mle_solver == "BFGS") {
      hglm_out$coef <- lm_bfgs(X, y)
      hglm_out$mle_solver <- "BGFS"
    }
    else {
      stop(sprintf("MLE solvers other than OLS and BFGS have not been implemented!"))
    }
  }
  return(hglm_out)
}
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
hiper_glm <- function(X, y, model = "linear", option = list()){
  supported_model <- c("linear","logit")
  if (!model %in% supported_model){
    stop(sprintf("The model %s is not supported!", model))
  }
  if (model == "linear"){
    hglm_out <- linear_regression(X, y, option)
  }
  else {
    hglm_out <- logistic_regression(X, y, option)
  }
  class(hglm_out) <- "hglm"
  return (hglm_out)
}

linear_regression <- function(X, y, option = list(mle_solver="OLS")){
  if (option$mle_solver == "OLS") {
    return(list(coef = lm_ols(X, y),
                mle_solver = "OLS"))
  }
  else if (option$mle_solver == "BFGS") {
    return(list(coef = lm_bfgs(X, y),
                mle_solver = "BFGS"))
  }
  else {
    stop(sprintf("MLE solvers other than OLS and BFGS have not been implemented!"))
  }
}

logistic_regression <- function(X, y, option = list(mle_solver="newton-raphson")){
  if (option$mle_solver == "newton-raphson") {
    return(list(coef = logistic_newton_raphson(X, y),
                mle_solver = "newton-raphson"))
  }
  else if (option$mle_solver == "BFGS") {
    return(list(coef = logistic_bfgs(X, y),
                mle_solver = "BFGS"))
  }
  else {
    stop(sprintf("MLE solvers other than Newton-Raphson and BFGS have not been implemented!"))
  }
}





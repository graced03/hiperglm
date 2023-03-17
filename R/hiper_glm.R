#' GLM function for high-dimensional data
#'
#' @param X the design matrix
#' @param y the outcome vector/matrix of the model
#' @param model choose the type of GLM, currently supporting linear regression or logistic regression
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
  hglm_out <- find_mle(X, y, model, option)
  class(hglm_out) <- "hglm"
  return (hglm_out)
}

find_mle <- function(X, y, model, option) {
  if (is.null(option$mle_solver)) {
    if (model == "linear"){
      return(list(coef = lm_ols(X, y), mle_solver = "OLS"))
    } else {
      return(list(coef = newton_raphson(X, y),
                  mle_solver = "newton-raphson"))
    }
  } else {
    return(list(coef = solve_via_bfgs(X, y, model, method = option$mle_solver), mle_solver = "BFGS"))
  }
}

solve_via_bfgs <- function(X, y, model, method) {
  init_coef <- rep(0, ncol(X))
  if (model == "linear") {
    fn <- function(betas) {
      lm_log_likelihood(X, y, betas)
    }
    grad <- function(betas) {
      lm_log_likelihood_grad(X, y, betas)
    }
  } else {
    fn <- function(betas) {
      logistic_log_likelihood(X, y, betas)
    }
    grad <- function(betas) {
      logistic_log_likelihood_grad(X, y, betas)
    }
  }
  bfgs_est <- stats::optim(par = init_coef,
                           fn = fn,
                           gr = grad,
                           method = method,
                           control=list(fnscale=-1))
  optim_converged <- (bfgs_est$convergence == 0L)
  if (!optim_converged) {
    warning("Optimization did not converge.")
  }
  return (bfgs_est$par)
}





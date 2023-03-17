logistic <- function(x){
  return ( exp(x) / (1+exp(x)) )
}

logistic_log_likelihood <- function(X, y, betas) {
  fitted <- X %*% betas
  return (sum((y * fitted) - log(1 + exp(fitted))))
}

logistic_log_likelihood_grad <- function(X, y, betas) {
  prob <- logistic(X %*% betas)
  return (t(X) %*% (y - prob))
}

logistic_hession <- function(X, betas){
  prob <- as.vector(logistic(X %*% betas))
  W <- diag(prob * (1 - prob))
  return (-t(X) %*% W %*% X)
}

newton_raphson <- function(X, y, optimizer, atol = 1e-8, rtol = 1e-8, iter_max = 25L) {
  betas <- rep(0, ncol(X))

  i <- 0L
  converged <- FALSE
  curr_log_likelihood <- logistic_log_likelihood(X, y, betas)

  while (i <= iter_max & converged == FALSE) {

    i <- i + 1L
    prev_log_likelihood <- curr_log_likelihood
    betas <- take_one_newton_step(X, y, betas, optimizer)
    curr_log_likelihood <- logistic_log_likelihood(X, y, betas)

    if (are_all_close(curr_log_likelihood, prev_log_likelihood, abs_tol = atol, rel_tol = rtol)) {
      converged <- TRUE
      break
    }
  }
  if (converged == FALSE) {
    warning(sprintf("Maximum iteration reached %d and the algorithm has not converged.", i))
  }
  return(betas)
}

take_one_newton_step <- function(X, y, betas, optimizer){
  if (is.null(optimizer)){
    optimizer_via_qr(X, y, betas)
  } else {
    solve_via_lu(X, y, betas)
  }
}

optimizer_via_qr <- function(X, y, betas){
  prob <- as.vector(logistic(X %*% betas))
  y_hat_var <- prob * (1 - prob)
  z <- log(prob) - log(1-prob) + (y - prob) / y_hat_var
  W_sqrt <- diag(sqrt(y_hat_var))
  X_tilda <- W_sqrt %*% X
  z_tilda <- W_sqrt %*% z
  return (qr.solve(X_tilda, z_tilda))
}
solve_via_lu <- function(X, y, betas){
  grad <- logistic_log_likelihood_grad(X, y, betas)
  hessian <- logistic_hession(X, betas)
  delta_betas <- solve(hessian, grad)
  return (betas - delta_betas)
}

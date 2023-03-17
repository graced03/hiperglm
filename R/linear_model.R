lm_ols <- function(X, y){
  return (solve(t(X) %*% X, t(X) %*% y))
}

solve_ols_via_qr <- function(X, y){
  return (qr_eigen(X,y))
}
lm_log_likelihood <- function(X, y, betas, noise_var = 1) {
  fitted <- X %*% betas
  residual <- y - fitted
  return (- sum(residual^2) / noise_var / 2)
}

lm_log_likelihood_grad <- function(X, y, betas, noise_var = 1){
  return (t(X) %*% y - t(X) %*% X %*% betas)
}



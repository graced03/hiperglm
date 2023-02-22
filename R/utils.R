#' Approximated gradient via finite difference method for multivariate input
#'
#' @param func function to approximate gradient
#' @param x variable in the function
#' @param dx delta x
#'
#' @return numerical gradients of the function
#'
approx_grad <- function(func, x, dx = .Machine$double.eps^(1/3)) {
  # finite difference for multivariate
  numerical_grad <- sapply(1:length(x), function(i) approx_partial_grad(i, func, x, dx))
  return(numerical_grad)
}

#' Partial gradient used in the finite difference method for multivariate input
#'
#' @param func function to approximate gradient
#' @param x variable in the function
#' @param dx delta x
#'
#' @return gradient of x_i
#'
approx_partial_grad <- function(i, func, x, dx = .Machine$double.eps^(1/3)) {
  delta_x <- rep(0, length(x))
  delta_x[i] <- dx
  return ((func(x + delta_x) - func(x - delta_x)) / (2*dx))
}

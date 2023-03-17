#include <RcppEigen.h>
#include "hiperglm_types.h"
// [[Rcpp::depends(RcppEigen)]]
//' @export
// [[Rcpp::export]]
VectorXd qr_eigen(Map<MatrixXd> A, Map<VectorXd> y) {
  if (A.rows() != y.size()) {
    Rcpp::stop("Incompatible matrix-vector dimensions.");
  }
  Eigen::HouseholderQR<Eigen::MatrixXd> qr(A);
  return qr.solve(y);
}

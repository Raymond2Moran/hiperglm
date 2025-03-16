#include "hiperglm_types.h"

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
VectorXd rcpp_qr_solve(const Map<MatrixXd> X, const Map<MatrixXd> y) {
    // Check dimensions
    if (X.rows() != y.rows()) {
        Rcpp::stop("Dimension mismatch: X has " + std::to_string(X.rows()) +
                   " rows, y has " + std::to_string(y.rows()) + " rows");
    }
    
    HouseholderQR<MatrixXd> qr(X);
    return qr.solve(y);
}

// [[Rcpp::export]]
MatrixXd rcpp_qr_R(const MatrixXd& X) {
    HouseholderQR<MatrixXd> qr(X);
    return qr.matrixQR().triangularView<Eigen::Upper>();
}

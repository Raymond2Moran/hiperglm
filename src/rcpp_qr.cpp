#include "hiperglm_types.h"

// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::export]]
Eigen::VectorXd rcpp_qr_solve(const Eigen::MatrixXd& X, const Eigen::VectorXd& y) {

    // Check dimensions
    if (X.rows() != y.rows()) {
        Rcpp::stop("Dimension mismatch: X has " + std::to_string(X.rows()) +
                   " rows, y has " + std::to_string(y.rows()) + " rows");
    }
    
    HouseholderQR<MatrixXd> qr(X);
    return qr.solve(y);
}

// [[Rcpp::export]]
Eigen::MatrixXd rcpp_qr_R(const Eigen::MatrixXd& X) {
    HouseholderQR<MatrixXd> qr(X);
    return qr.matrixQR().triangularView<Eigen::Upper>();
}

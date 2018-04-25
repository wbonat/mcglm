#include <RcppArmadillo.h>
// [[Rcpp::depends("RcppArmadillo")]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
arma::sp_mat mc_sensitivity_op(List products) {
  int n_par = products.size();
  sp_mat S = sp_mat(n_par, n_par);
  for (int i = 0; i <= n_par - 1; i++) {
    sp_mat TEMP = as<sp_mat>(products[i]).t();
    for (int j = 0; j <= i; j++) {
      S(i,j) = -accu(TEMP % as<sp_mat>(products[j]));
    }
  }
  return S;
}

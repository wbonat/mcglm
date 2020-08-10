#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends("RcppArmadillo")]]
// [[Rcpp::export]]
arma::sp_mat mc_variability_op(List P2, arma::sp_mat C, List WE, arma::vec k4, arma::vec W) {
  int n_par = WE.size();
  sp_mat V = sp_mat(n_par, n_par);
  for (int i = 0; i <= n_par - 1; i++) {
    vec Wi;
    Wi = as<sp_mat>(WE[i]).diag();
    sp_mat TEMP = as<sp_mat>(P2[i]) * C;
    TEMP = TEMP.t();
    for (int j = 0; j <= i; j++) {
      vec Wj;
      Wj = as<sp_mat>(WE[j]).diag();
      sp_mat TEMPO = as<sp_mat>(P2[j]) * C;
      V(i,j) = 2 * accu(TEMP % TEMPO) + sum(k4 % Wi % W % Wj % W);
    }
  }
  return V;
}

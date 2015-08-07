#' Derivatives of V^{1/2} with respect to beta.
#'
#' @description Compute the derivatives of V^{1/2} matrix with respect to the regression
#' parameters beta.
#' @param D A matrix.
#' @param D_V_sqrt_mu A matrix.
#' @param Omega A matrix.
#' @param V_sqrt A matrix.
#' @param variance A string specifying the variance function name.
#' @return A list of matrices, containg the derivatives of V^{1/2} with respect to the regression
#' parameters.
#' @export
mc_derivative_sigma_beta <- function(D, D_V_sqrt_mu, Omega, V_sqrt, variance) {
  n_beta <- dim(D)[2]
  n_obs <- dim(D)[1]
  output <- list()
  if(variance == "power" | variance == "binomialP" | variance == "binomialPQ"){
  for(i in 1:n_beta) {
    D_V_sqrt_beta <-  Diagonal(n_obs, D_V_sqrt_mu*D[,i])
    output[[i]] <- mc_sandwich_power(middle = Omega,
                                     bord1 = V_sqrt, bord2 = D_V_sqrt_beta)
    }
  }
  if(variance == "poisson_tweedie") {
    for(i in 1:n_beta){
      D_V_sqrt_beta <-  Diagonal(n_obs, D_V_sqrt_mu*D[,i])
      output[[i]] <- Diagonal(n_obs, D[,i]) + mc_sandwich_power(middle = Omega,
                                       bord1 = V_sqrt, bord2 = D_V_sqrt_beta)
      }
    }
  return(output)
}

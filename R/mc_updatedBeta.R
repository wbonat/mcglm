#' @title Updated regression parameters
#' @author Wagner Hugo Bonat
#'
#' @description This function update a list of regression parameters. It
#'     will be useful only inside the fitting algorithm.
#'
#' @param list_initial A list of initial values.
#' @param betas A vector with actual regression parameters values.
#' @param information A list with information about the number of
#'     parameters in the model. In general the output from
#'     \link{mc_getInformation}.
#' @keywords internal
#' @param n_resp A numeric specyfing the number of response variables.
#' @return A list with updated values of the regression parameters.

mc_updateBeta <- function(list_initial, betas, information, n_resp) {
    cod <- rep(1:n_resp, information$n_betas)
    temp <- data.frame(beta = betas, cod)
    for (k in 1:n_resp) {
        list_initial$regression[[k]] <-
            temp[which(temp$cod == k), ]$beta
    }
    return(list_initial)
}

#' @title Updated covariance parameters
#' @author Wagner Hugo Bonat
#'
#' @description This function update a list of covariance parameters. It
#'     will be useful only inside the fitting algorithm.
#'
#' @param list_initial A list of initial values.
#' @param covariance A vector with actual covariance parameters values.
#' @param information A list with information about the number of
#'     parameters in the model. In general the output from
#'     \link{mc_getInformation}.
#' @param list_power_fixed A list of logicals indicating if the power
#'     parameter should be estimated or not.
#' @param n_resp A numeric specyfing the number of response variables.
#' @keywords internal
#' @return A list with updated values of the covariance parameters.

mc_updateCov <- function(list_initial, covariance, list_power_fixed,
                         information, n_resp) {
    rho_cod <- rep("rho", information$n_rho)
    tau_cod <- list()
    power_cod <- list()
    for (i in 1:n_resp) {
        power_cod[[i]] <- rep(paste("power", i, sep = ""),
                              information$n_power[[i]])
        tau_cod[[i]] <- rep(paste("tau", i, sep = ""),
                            information$n_tau[[i]])
    }
    temp <- data.frame(values = covariance,
                       cod = c(rho_cod,
                               do.call(c, Map(c, power_cod, tau_cod))))
    cod.tau <- paste("tau", 1:n_resp, sep = "")
    for (i in 1:n_resp) {
        list_initial$tau[[i]] <-
            temp[which(temp$cod == cod.tau[i]), ]$values
    }
    cod.power <- paste("power", 1:n_resp, sep = "")
    for (i in 1:n_resp) {
        if (list_power_fixed[[i]] == FALSE) {
            list_initial$power[[i]] <-
                temp[which(temp$cod == cod.power[i]), ]$values
        }
    }
    if (length(information$n_betas) != 1) {
        list_initial$rho <-
            temp[which(temp$cod == "rho"), ]$values
    }
    return(list_initial)
}

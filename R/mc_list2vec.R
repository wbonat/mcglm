#' @title Auxiliar function transforms list to a vector.
#' @author Wagner Hugo Bonat
#'
#' @description This function takes a list of parameters and tranforms
#'     to a vector.
#'
#' @param list_initial A list specifying initial values.
#' @param list_power_fixed A list of logical operators specyfing if the
#'     power parameter should be estimated or not.
#' @keywords internal
#' @details It is an internal function, in general the users never will
#'     use this function.  It will be useful, only if the user wants to
#'     implement a different variance-covariance matrix.
#' @return A vector of model parameters.

mc_list2vec <- function(list_initial, list_power_fixed) {
    cov_ini <- do.call(c, Map(c, list_initial$power, list_initial$tau))
    n_resp <- length(list_initial$regression)
    indicadora <- list()
    for (i in 1:n_resp) {
        indicadora[[i]] <-
            rep(FALSE, length(list_initial$tau[[i]]))
    }
    indicadora_power <- list()
    for (i in 1:n_resp) {
        if (list_power_fixed[[i]] == FALSE) {
            indicadora_power[[i]] <-
                rep(FALSE, length(list_initial$power[[i]]))
        }
        if (list_power_fixed[[i]] == TRUE) {
            indicadora_power[[i]] <-
                rep(TRUE, length(list_initial$power[[i]]))
        }
    }
    index <- do.call(c, Map(c, indicadora_power, indicadora))
    cov_par <- data.frame(cov_ini, index)
    cov_ini <- cov_par[which(cov_par$index == FALSE), ]$cov_ini
    beta_ini <- do.call(c, list_initial$regression)
    cov_ini <- c(rho = list_initial$rho, cov_ini)
    return(list(beta_ini = as.numeric(beta_ini),
                cov_ini = as.numeric(cov_ini)))
}

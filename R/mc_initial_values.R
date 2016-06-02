#' @title Automatic initial values for McGLMs.
#' @author Wagner Hugo Bonat
#'
#' @description This function provides o list of initial values to be
#'     used while fitting McGLMs.
#'
#' @param linear_pred A list of formula see \code{\link[stats]{formula}}
#'     for details.
#' @param matrix_pred A list of known matrices to be used on the matrix
#'     linear predictor. Details can be obtained on
#'     \code{\link[mcglm]{mc_matrix_linear_predictor}}.
#' @param link A list of link functions names, see
#'     \code{\link[mcglm]{mc_link_function}} for details.
#' @param variance A list of variance functions names, see
#'     \code{\link[mcglm]{mc_variance_function}} for details.
#' @param covariance A list of covariance link functions names, current
#'     options are: identity, inverse and exponential-matrix (expm).
#' @param offset A list with values of offset values if any.
#' @param Ntrial A list with values of the number of trials on Bernoulli
#'     experiments. It is useful only for binomialP and binomialPQ
#'     variance functions.
#' @param contrasts List of contrasts to be used in the
#'     \code{\link[stats]{model.matrix}}.
#' @param data A data frame.
#' @return Return a list of initial values to be used while fitting
#'     McGLMs.
#' @usage mc_initial_values(linear_pred, matrix_pred, link, variance,
#'                   covariance, offset, Ntrial, contrasts, data)
#' @export

mc_initial_values <- function(linear_pred, matrix_pred, link,
                              variance, covariance, offset,
                              Ntrial, contrasts = NULL, data) {
    n_resp <- length(linear_pred)
    if (!is.null(contrasts)) {
        list_X <- list()
        for (i in 1:n_resp) {
            list_X[[i]] <- model.matrix(linear_pred[[i]],
                                        contrasts = contrasts[[i]],
                                        data = data)
        }
    } else {
        list_X <- lapply(linear_pred, model.matrix, data = data)
    }
    list_models <- list()
    power_initial <- list()
    for (i in 1:n_resp) {
        if (variance[[i]] == "constant") {
            power_initial[[i]] <- 0
            if (!is.null(offset[[i]])) {
                data_temp <- data
                data_temp$offset <- offset[[i]]
                list_models[[i]] <-
                    glm(linear_pred[[i]],
                        family = quasi(link = link[[i]],
                                       variance =
                                           "constant"), offset = offset,
                        data = data_temp)
            } else {
                list_models[[i]] <-
                    glm(linear_pred[[i]],
                        family = quasi(link = link[[i]],
                                       variance = "constant"),
                        data = data)
            }
        }
        if (variance[[i]] == "tweedie" |
                variance[[i]] == "poisson_tweedie") {
            power_initial[[i]] <- 1
            if (!is.null(offset[[i]])) {
                data_temp <- data
                data_temp$offset <- offset[[i]]
                list_models[[i]] <-
                    glm(linear_pred[[i]],
                        family = quasi(link = link[[i]],
                                       variance = "mu"),
                        offset = offset, data = data_temp)
            } else {
                list_models[[i]] <-
                    glm(linear_pred[[i]],
                        family = quasi(link = link[[i]],
                                       variance = "mu"), data = data)
            }
        }
        if (variance[[i]] == "binomialP" |
                variance[[i]] == "binomialPQ") {
            power_initial[[i]] <- c(1)
            if (variance[[i]] == "binomialPQ") {
                power_initial[[i]] <- c(1, 1)
            }
            if (!is.null(Ntrial[[i]])) {
                temp <- model.frame(linear_pred[[i]], data = data)
                Y <- model.response(temp) * Ntrial[[i]]
                resp <- cbind(Y, Ntrial[[i]] - Y)
                X <- model.matrix(linear_pred[[i]], data = data)
                link_temp <- link[[i]]
                if (link_temp == "loglog") {
                    link_temp <- "cloglog"
                }
                list_models[[i]] <-
                    glm(resp ~ X - 1,
                        family = binomial(link = link_temp),
                        data = data)
            } else {
                link_temp <- link[[i]]
                if (link_temp == "loglog") {
                    link_temp <- "cloglog"
                }
                list_models[[i]] <-
                    glm(linear_pred[[i]],
                        family = quasi(link = link_temp,
                                       variance = "mu(1-mu)"),
                        data = data)
            }
        }
    }
    list_initial <- list()
    list_initial$regression <- lapply(list_models, coef)
    list_initial$power <- power_initial
    tau0_initial <- lapply(list_models,
                           function(x) summary(x)$dispersion)
    tau_extra <- lapply(matrix_pred, length)
    list_initial$tau <- list()
    for (i in 1:n_resp) {
        if (covariance[i] == "identity") {
            list_initial$tau[[i]] <-
                as.numeric(c(tau0_initial[[i]],
                             rep(0, c(tau_extra[[i]] - 1))))
        }
        if (covariance[i] == "inverse") {
            list_initial$tau[[i]] <-
                as.numeric(c(1/tau0_initial[[i]],
                             rep(0, c(tau_extra[[i]] - 1))))
        }
        if (covariance[i] == "expm") {
            list_initial$tau[[i]] <-
                as.numeric(c(exp(tau0_initial[[i]]),
                             rep(0.1, c(tau_extra[[i]] - 1))))
        }
    }
    if (n_resp == 1) {
        list_initial$rho <- 0
    } else {
        list_initial$rho <- rep(0, n_resp * (n_resp - 1)/2)
    }
    return(list_initial)
}

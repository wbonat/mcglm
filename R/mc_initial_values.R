#' @title Automatic Initial Values
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description This function provides initial values to be used when
#' fitting multivariate covariance generalized linear models by using
#' the function \code{mcglm}. In general the users do not need to use
#' this function, since it is already employed when setting the argument
#' \code{control_initial = "automatic"} in the \code{mcglm} function.
#' However, if the users want to change some of the initial values,
#' this function can be useful.
#'
#' @param linear_pred a list of formula see \code{\link[stats]{formula}}
#'     for details.
#' @param matrix_pred a list of known matrices to be used on the matrix
#'     linear predictor. \cr
#'     See \code{\link[mcglm]{mc_matrix_linear_predictor}} for details.
#' @param link a list of link functions names, see
#'     \code{\link[mcglm]{mcglm}} for details.
#' @param variance a list of variance functions names, see
#'     \code{\link[mcglm]{mcglm}} for details.
#' @param covariance a list of covariance link functions names, see
#'    \code{\link[mcglm]{mcglm}} for details.
#' @param offset a list of offset values if any.
#' @param Ntrial a list of the number of trials on Bernoulli
#'     experiments. It is useful only for \code{"binomialP"} and
#'     \code{"binomialPQ"} variance functions.
#' @param contrasts list of contrasts to be used in the
#'     \code{\link[stats]{model.matrix}}.
#' @param data data frame.
#' @return Return a list of initial values to be used while fitting
#'     in the \code{mcglm} function.
#'
#' @details To obtain initial values for multivariate covariance
#' generalized linear models the function \cr \code{mc_initial_values} fits
#' a generalized linear model (GLM) using the function \code{glm} with
#' the specified linear predictor and link function for each response
#' variables considering independent observations. The \code{family}
#' argument is always specified as \code{quasi}. The link function depends
#' on the specification of the argument \code{link}.
#' The variance function is always specified as \code{"mu"} the only
#' excession appears when using \code{variance = "constant"} then the
#' family argument in the \code{glm} function is specified as
#' \code{quasi(link = link, variance = "constant")}. The estimated value
#' of the dispersion parameter from the \code{glm} function is used as
#' initial value for the first component of the matrix linear predictor,
#' for all other components the value zero is used. \cr For the
#' cases \code{covariance = "inverse"} and \code{covariance = "expm"}
#' the inverse and the logarithm of the estimated dispersion parameter
#' is used as initial value for the first component of the matrix linear
#' predictor. The value of the power parameter is always started at 1.
#' In the cases of multivariate models the correlation between response
#' variables is always started at 0.
#'
#' @usage mc_initial_values(linear_pred, matrix_pred, link, variance,
#'                   covariance, offset, Ntrial, contrasts, data)
#'
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
                as.numeric(c(log(tau0_initial[[i]]),
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

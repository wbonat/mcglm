#' Extract model coefficients for mcglm class
#'
#' coef.mcglm is a function which extracts model coefficients from objects of mcglm class.
#' @param object An object of mcglm class.
#' @param std.error Logical. Returns or not the standard errors.
#' @param response A numeric or vector specyfing for which response variables the coefficients
#' should be returned.
#' @param type A string or string vector specyfing which coefficients should be returned.
#' Options are 'beta', 'tau', 'power', 'tau' and 'correlation'.
#' @param ... additional arguments affecting the summary produced. Note that there is no extra options for
#' mcglm object class.
#' @return A data.frame with estimates, parameters names, response number and parameters type.
#' @export

coef.mcglm <- function(object, std.error = FALSE, response = c(NA, 1:length(object$beta_names)),
                       type = c("beta", "tau", "power", "correlation"), ...) {
    n_resp <- length(object$beta_names)
    cod_beta <- list()
    cod_power <- list()
    cod_tau <- list()
    type_beta <- list()
    type_power <- list()
    type_tau <- list()
    resp_beta <- list()
    resp_power <- list()
    resp_tau <- list()
    response_for <- 1:n_resp
    for (i in response_for) {
        cod_beta[[i]] <- paste(paste("beta", i, sep = ""), 0:c(object$Information$n_betas[[i]] - 1), sep = "")
        type_beta[[i]] <- rep("beta", length(cod_beta[[i]]))
        resp_beta[[i]] <- rep(response_for[i], length(cod_beta[[i]]))
        if (object$Information$n_power[[i]] != 0 | object$power_fixed[[i]] == FALSE) {
            cod_power[[i]] <- paste(paste("power", i, sep = ""), 1:object$Information$n_power[[i]], sep = "")
            type_power[[i]] <- rep("power", length(cod_power[[i]]))
            resp_power[[i]] <- rep(response_for[i], length(cod_power[[i]]))
        }
        if (object$Information$n_power[[i]] == 0) {
            cod_power[[i]] <- rep(1, 0)
            type_power[[i]] <- rep(1, 0)
            resp_power[[i]] <- rep(1, 0)
        }
        cod_tau[[i]] <- paste(paste("tau", i, sep = ""), 1:object$Information$n_tau[[i]], sep = "")
        type_tau[[i]] <- rep("tau", length(cod_tau[[i]]))
        resp_tau[[i]] <- rep(response_for[i], length(cod_tau[[i]]))
    }
    rho_names <- c()
    if (n_resp != 1) {
        combination <- combn(n_resp, 2)
        for (i in 1:dim(combination)[2]) {
            rho_names[i] <- paste(paste("rho", combination[1, i], sep = ""), combination[2, i], sep = "")
        }
    }
    type_rho <- rep("correlation", length(rho_names))
    resp_rho <- rep(NA, length(rho_names))
    cod <- c(do.call(c, cod_beta), rho_names, do.call(c, Map(c, cod_tau)))
    type_cod <- c(do.call(c, type_beta), type_rho, do.call(c, Map(c, type_tau)))
    response_cod <- c(do.call(c, resp_beta), resp_rho, do.call(c, Map(c, resp_tau)))

    if (length(cod_power) != 0) {
        cod <- c(do.call(c, cod_beta), rho_names, do.call(c, Map(c, cod_power, cod_tau)))
        type_cod <- c(do.call(c, type_beta), type_rho, do.call(c, Map(c, type_power, type_tau)))
        response_cod <- c(do.call(c, resp_beta), resp_rho, do.call(c, Map(c, resp_power, resp_tau)))
    }

    Estimates <- c(object$Regression, object$Covariance)
    coef_temp <- data.frame(Estimates = Estimates, Parameters = cod, Type = type_cod, Response = response_cod)
    if (std.error == TRUE) {
        coef_temp <- data.frame(Estimates = Estimates, Std.error = sqrt(diag(object$vcov)), Parameters = cod, Type = type_cod,
            Response = response_cod)
    }

    output <- coef_temp[which(coef_temp$Response %in% response & coef_temp$Type %in% type), ]
    return(output)
}

#' @title Fitting Multivariate Covariance Generalized Linear Models.
#' @author Wagner Hugo Bonat
#'
#' @description The function \code{mcglm} is used to fit multivariate
#'     covariance generalized linear models.
#'     The models are specified by a set of lists giving a symbolic
#'     description of the linear predictor.
#'     The user can choose between a list of link, variance and covariance
#'     functions. The models are fitted using an estimating function
#'     approach, combining quasi-score functions for regression
#'     parameters and Pearson estimating function for covariance
#'     parameters. For details see Bonat and Jorgensen (2016).
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
#' @param power_fixed A list of logicals indicating if the values of the
#'     power parameter should be estimated or not.
#' @param control_initial A list of initial values for the fitting
#'     algorithm. See details below.
#' @param control_algorithm A list of arguments to be passed for the
#'     fitting algorithm. See \code{\link[mcglm]{fit_mcglm}} for
#'     details.
#' @param contrasts Extra arguments to passed to
#'     \code{\link[stats]{model.matrix}}.
#' @param data A dta frame.
#' @return mcglm returns an object of class 'mcglm'.
#' @export
#' @import Matrix

mcglm <- function(linear_pred, matrix_pred, link, variance,
                  covariance, offset, Ntrial, power_fixed,
                  data, control_initial = "automatic",
                  contrasts = NULL,
                  control_algorithm = list()) {
    n_resp <- length(linear_pred)
    linear_pred <- as.list(linear_pred)
    matrix_pred <- as.list(matrix_pred)
    if (missing(link)) {
        link <- rep("identity", n_resp)
    }
    if (missing(variance)) {
        variance <- rep("constant", n_resp)
    }
    if (missing(covariance)) {
        covariance <- rep("identity", n_resp)
    }
    if (missing(offset)) {
        offset <- rep(list(NULL), n_resp)
    }
    if (missing(Ntrial)) {
        Ntrial <- rep(list(rep(1, dim(data)[1])), n_resp)
    }
    if (missing(power_fixed)) {
        power_fixed <- rep(TRUE, n_resp)
    }
    if (missing(contrasts)) {
        constrasts <- NULL
    }
    link <- as.list(link)
    variance <- as.list(variance)
    covariance <- as.list(covariance)
    offset <- as.list(offset)
    Ntrial <- as.list(Ntrial)
    power_fixed <- as.list(power_fixed)
    if (class(control_initial) != "list") {
        control_initial <-
            mc_initial_values(linear_pred = linear_pred,
                              matrix_pred = matrix_pred, link = link,
                              variance = variance,
                              covariance = covariance, offset = offset,
                              Ntrial = Ntrial, contrasts = contrasts,
                              data = data)
        cat("Automatic initial values selected.", "\n")
    }
    con <- list(correct = TRUE, max_iter = 20, tol = 1e-04,
                method = "chaser", tunning = 1, verbose = FALSE)
    con[(namc <- names(control_algorithm))] <- control_algorithm
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

    list_model_frame <- lapply(linear_pred, model.frame, data = data)
    list_Y <- lapply(list_model_frame, model.response)
    y_vec <- as.numeric(do.call(c, list_Y))
    sparse <- lapply(matrix_pred, function(x) {
        if (class(x) == "dgeMatrix") {
            FALSE
        } else TRUE
    })
    model_fit <- try(fit_mcglm(list_initial = control_initial,
                               list_link = link,
                               list_variance = variance,
                               list_covariance = covariance,
                               list_X = list_X, list_Z = matrix_pred,
                               list_offset = offset,
                               list_Ntrial = Ntrial,
                               list_power_fixed = power_fixed,
                               list_sparse = sparse, y_vec = y_vec,
                               correct = con$correct,
                               max_iter = con$max_iter, tol = con$tol,
                               method = con$method,
                               tunning = con$tunning,
                               verbose = con$verbose))
    if (class(model_fit) != "try-error") {
        model_fit$beta_names <- lapply(list_X, colnames)
        model_fit$power_fixed <- power_fixed
        model_fit$list_initial <- control_initial
        model_fit$n_obs <- dim(data)[1]
        model_fit$link <- link
        model_fit$variance <- variance
        model_fit$covariance <- covariance
        model_fit$linear_pred <- linear_pred
        model_fit$con <- con
        model_fit$observed <- Matrix(y_vec, ncol = length(list_Y),
                                     nrow = dim(data)[1])
        model_fit$list_X <- list_X
        model_fit$matrix_pred <- matrix_pred
        model_fit$Ntrial <- Ntrial
        model_fit$offset <- offset
        model_fit$power_fixed
        model_fit$sparse <- sparse
        model_fit$data <- data
        class(model_fit) <- "mcglm"
    }
    return(model_fit)
}

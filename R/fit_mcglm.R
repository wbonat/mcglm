#' @title Chaser and Reciprocal Likelihood Algorithms
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description This function implements the two main algorithms used
#' for fitting multivariate covariance generalized linear models.
#' The chaser and the reciprocal likelihood algorithms.
#'
#' @param list_initial a list of initial values for regression and
#' covariance parameters.
#' @param list_link a list specifying the link function names. \cr
#' Options are: \code{"logit"}, \code{"probit"}, \code{"cauchit"},
#' \code{"cloglog"}, \code{"loglog"}, \code{"identity"}, \code{"log"},
#' \code{"sqrt"}, \code{"1/mu^2"} and \code{"inverse"}. \cr
#' See \code{\link{mc_link_function}} for details.
#' @param list_variance a list specifying the variance function names.
#' Options are: \code{"constant"}, \code{"tweedie"},
#' \code{"poisson_tweedie"}, \code{"binomialP"} and \code{"binomialPQ"}.
#' See \code{\link{mc_variance_function}} for details.
#' @param list_covariance a list of covariance function names. Options
#' are: \code{"identity"}, \code{"inverse"} and \code{"expm"}.
#' @param list_X a list of design matrices.
#' See \code{\link[stats]{model.matrix}} for details.
#' @param list_Z a list of knowm matrices to compose the matrix linear
#' predictor.
#' @param list_offset a list of offset values.
#' @param list_Ntrial a list of number of trials, useful only when
#' analysing binomial data.
#' @param list_power_fixed a list of logicals indicating if the power
#' parameters should be estimated or not.
#' @param list_sparse a list of logicals indicating if the matrices
#' should be set up as sparse matrices. This argument is useful only
#' when using exponential-matrix covariance link function.
#' In the other cases the algorithm detects automatically if the matrix
#' should be sparse or not.
#' @param y_vec a vector of the stacked response variables.
#' @param correct a logical indicating if the algorithm will use the
#' correction term or not.
#' @param max_iter maximum number of iterations.
#' @param tol a numeric specyfing the tolerance.
#' @param method a string specyfing the method used to fit the models
#' (\code{"chaser"} or \code{"rc"}).
#' @param tunning a numeric value in general close to zero for the rc
#' method and close to 1 for the chaser method. This argument control
#' the step-length.
#' @param verbose a logical if TRUE print the values of the covariance
#' parameters used on each iteration.
#' @usage fit_mcglm(list_initial, list_link, list_variance,
#'          list_covariance, list_X, list_Z, list_offset,
#'          list_Ntrial, list_power_fixed, list_sparse,
#'          y_vec, correct, max_iter, tol, method,
#'          tunning, verbose)
#' @return A list with estimated regression and covariance parameters.
#' Details about the estimation procedures as iterations, sensitivity,
#' variability are also provided. In general the users do not need to
#' use this function directly. The \code{\link{mcglm}} provides GLM
#' interface for fitting \code{mcglm} .
#' @seealso \code{mcglm}, \code{mc_matrix_linear_predictor},
#'  \code{mc_link_function} and \cr \code{mc_variance_function}.
#'
#' @source Bonat, W. H. and Jorgensen, B. (2016) Multivariate
#'     covariance generalized linear models.
#'     Journal of Royal Statistical Society - Series C X(X):XX--XX.
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#'     Models in R: The mcglm Package.
#'     Journal of Statistical Software, submitted.
#'
#' @importFrom stats as.formula binomial coef dist fitted glm make.link
#' model.frame model.matrix na.exclude pchisq qchisq qnorm quasi
#' residuals vcov model.response
#' @importFrom utils combn
#' @importFrom grDevices dev.new
#' @export

fit_mcglm <- function(list_initial, list_link, list_variance,
                      list_covariance, list_X, list_Z,
                      list_offset, list_Ntrial, list_power_fixed,
                      list_sparse, y_vec,
                      correct = TRUE, max_iter, tol = 0.001,
                      method = "rc",
                      tunning = 0, verbose) {
    ## Transformation from list to vector
    parametros <- mc_list2vec(list_initial, list_power_fixed)
    n_resp <- length(list_initial$regression)
    if (n_resp == 1) {
        parametros$cov_ini <- parametros$cov_ini[-1]
    }
    ## Getting information about the number of parameters
    inf <- mc_getInformation(list_initial, list_power_fixed, n_resp = n_resp)
    ## Creating a matrix to sote all values used in the fitting step
    solucao_beta <- matrix(NA, max_iter, length(parametros$beta_ini))
    solucao_cov <- matrix(NA, max_iter, length(parametros$cov_ini))
    score_beta_temp <- matrix(NA, max_iter, length(parametros$beta_ini))
    score_disp_temp <- matrix(NA, max_iter, length(parametros$cov_ini))
    ## Setting the initial values
    solucao_beta[1, ] <- parametros$beta_ini
    solucao_cov[1, ] <- parametros$cov_ini
    beta_ini <- parametros$beta_ini
    cov_ini <- parametros$cov_ini
    for (i in 2:max_iter) {
        ## Step 1 - Quasi-score function Step 1.1 - Computing the mean structure
        mu_list <- Map(mc_link_function, beta = list_initial$regression,
                       offset = list_offset, X = list_X, link = list_link)
        mu_vec <- do.call(c, lapply(mu_list, function(x) x$mu))
        D <- bdiag(lapply(mu_list, function(x) x$D))
        # Step 1.2 - Computing the inverse of C matrix.
        # I should improve this step.
        # I have to write a new function to compute
        # only C or inv_C to be more efficient in this step.
        Cfeatures <- mc_build_C(list_mu = mu_list,
                                list_Ntrial = list_Ntrial,
                                rho = list_initial$rho,
                                list_tau = list_initial$tau,
            list_power = list_initial$power, list_Z = list_Z,
            list_sparse = list_sparse, list_variance = list_variance,
            list_covariance = list_covariance,
            list_power_fixed = list_power_fixed, compute_C = FALSE,
            compute_derivative_beta = FALSE,
            compute_derivative_cov = FALSE)
        # Step 1.3 - Update the regression parameters
        beta_temp <- mc_quasi_score(D = D, inv_C = Cfeatures$inv_C,
                                    y_vec = y_vec, mu_vec = mu_vec)
        solucao_beta[i, ] <- as.numeric(beta_ini - solve(beta_temp$Sensitivity, beta_temp$Score))
        score_beta_temp[i, ] <- as.numeric(beta_temp$Score)
        list_initial <- mc_updateBeta(list_initial, solucao_beta[i, ],
                                      information = inf, n_resp = n_resp)
        # Step 1.4 - Updated the mean structure to use in the Pearson
        # estimating function step.
        mu_list <- Map(mc_link_function, beta = list_initial$regression,
                       offset = list_offset, X = list_X, link = list_link)
        mu_vec <- do.call(c, lapply(mu_list, function(x) x$mu))
        D <- bdiag(lapply(mu_list, function(x) x$D))
        # Step 2 - Updating the covariance parameters
        Cfeatures <- mc_build_C(list_mu = mu_list,
                                list_Ntrial = list_Ntrial,
                                rho = list_initial$rho,
                                list_tau = list_initial$tau,
            list_power = list_initial$power, list_Z = list_Z,
            list_sparse = list_sparse, list_variance = list_variance,
            list_covariance = list_covariance,
            list_power_fixed = list_power_fixed, compute_C = TRUE,
            compute_derivative_beta = FALSE)
        # Step 2.1 - Using beta(i+1)
        #beta_temp2 <- mc_quasi_score(D = D, inv_C = Cfeatures$inv_C,
        #y_vec = y_vec, mu_vec =  mu_vec)
        inv_J_beta <- solve(beta_temp$Sensitivity)
        if (method == "chaser") {
            cov_temp <- mc_pearson(y_vec = y_vec, mu_vec = mu_vec,
                                   Cfeatures = Cfeatures,
                                   inv_J_beta = inv_J_beta, D = D,
                correct = correct, compute_variability = TRUE)
            step <- tunning * solve(cov_temp$Sensitivity, cov_temp$Score)
        }
        if (method == "rc") {
            cov_temp <- mc_pearson(y_vec = y_vec, mu_vec = mu_vec,
                                   Cfeatures = Cfeatures,
                                   inv_J_beta = inv_J_beta, D = D,
                                   correct = correct,
                                   compute_variability = TRUE)
            step <- solve(tunning * cov_temp$Score %*% t(cov_temp$Score)
                          %*% solve(cov_temp$Variability) %*%
                            cov_temp$Sensitivity + cov_temp$Sensitivity)%*% cov_temp$Score
        }
        ## Step 2.2 - Updating the covariance parameters
        score_disp_temp[i, ] <- cov_temp$Score
        cov_next <- as.numeric(cov_ini - step)
        list_initial <- mc_updateCov(list_initial = list_initial,
                                     list_power_fixed = list_power_fixed,
                                     covariance = cov_next,
                                     information = inf, n_resp = n_resp)
        ## print the parameters values
        if (verbose == TRUE) {
            print(round(cov_next, 4))
        }
        if (verbose == TRUE) {
            print(round(as.numeric(cov_temp$Score), 4))
        }
        ## Step 2.3 - Updating the initial values for the next step
        beta_ini <- solucao_beta[i, ]
        cov_ini <- cov_next
        solucao_cov[i, ] <- cov_next
        ## Checking the convergence
        tolera <- abs(c(solucao_beta[i, ], solucao_cov[i, ]) - c(solucao_beta[i - 1, ], solucao_cov[i - 1, ]))
        # if(verbose == TRUE){print(round(tolera, 4))}
        if (all(tolera <= tol) == TRUE)
            break
    }
    mu_list <- Map(mc_link_function, beta = list_initial$regression,
                   offset = list_offset, X = list_X, link = list_link)
    mu_vec <- do.call(c, lapply(mu_list, function(x) x$mu))
    D <- bdiag(lapply(mu_list, function(x) x$D))
    Cfeatures <- mc_build_C(list_mu = mu_list, list_Ntrial = list_Ntrial,
                            rho = list_initial$rho,
                            list_tau = list_initial$tau,
                            list_power = list_initial$power,
                            list_Z = list_Z, list_sparse = list_sparse,
                            list_variance = list_variance,
                            list_covariance = list_covariance,
                            list_power_fixed = list_power_fixed,
                            compute_C = TRUE,
                            compute_derivative_beta = FALSE)
    beta_temp2 <- mc_quasi_score(D = D, inv_C = Cfeatures$inv_C,
                                 y_vec = y_vec, mu_vec = mu_vec)
    inv_J_beta <- solve(beta_temp2$Sensitivity)

    cov_temp <- mc_pearson(y_vec = y_vec, mu_vec = mu_vec,
                           Cfeatures = Cfeatures, inv_J_beta = inv_J_beta,
                           D = D, correct = correct,
                           compute_variability = TRUE)
    Product_beta <- lapply(Cfeatures$D_C_beta, mc_multiply,
                           bord2 = Cfeatures$inv_C)
    S_cov_beta <- mc_cross_sensitivity(Product_cov = cov_temp$Extra,
                                       Product_beta = Product_beta,
                                       n_beta_effective = length(beta_temp$Score))
    res <- y_vec - mu_vec
    V_cov_beta <- mc_cross_variability(Product_cov = cov_temp$Extra,
                                       inv_C = Cfeatures$inv_C, res = res, D = D)
    p1 <- rbind(beta_temp2$Variability, t(V_cov_beta))
    p2 <- rbind(V_cov_beta, cov_temp$Variability)
    joint_variability <- cbind(p1, p2)
    inv_S_beta <- inv_J_beta
    inv_S_cov <- solve(cov_temp$Sensitivity)
    mat0 <- Matrix(0, ncol = dim(S_cov_beta)[1], nrow = dim(S_cov_beta)[2])
    cross_term <- -inv_S_cov %*% S_cov_beta %*% inv_S_beta
    p1 <- rbind(inv_S_beta, cross_term)
    p2 <- rbind(mat0, inv_S_cov)
    joint_inv_sensitivity <- cbind(p1, p2)
    VarCov <- joint_inv_sensitivity %*% joint_variability %*% t(joint_inv_sensitivity)
    output <- list(IterationRegression = solucao_beta,
                   IterationCovariance = solucao_cov,
                   ScoreRegression = score_beta_temp,
                   ScoreCovariance = score_disp_temp,
                   Regression = beta_ini,
                   Covariance = cov_ini,
                   vcov = VarCov, fitted = mu_vec,
                   residuals = res, inv_C = Cfeatures$inv_C,
                   C = Cfeatures$C, Information = inf,
                   mu_list = mu_list, inv_S_beta = inv_S_beta,
                   joint_inv_sensitivity = joint_inv_sensitivity,
                   joint_variability = joint_variability)
    return(output)
}

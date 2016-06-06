#' @title Score Information Criterion - Regression
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Compute the score information criterion (SIC) for an
#' object of \code{mcglm} class.
#' The SIC is useful for selecting the components of the linear predictor.
#' It can be used to construct an stepwise covariate selection.
#'
#' @param object an object of \code{mcglm} class.
#' @param scope a vector of covariate names to be tested.
#' @param data data set containing all variables involved in the model.
#' @param penalty penalty term (default = 2).
#' @param response index indicating for which response variable the
#' SIC should be computed.
#'
#' @source Bonat, W. H. (2016). Multiple Response Variables Regression
#' Models in R: The mcglm Package. Journal of Statistical Software, submitted.
#'
#' @source Bonat, et. al. (2016). Modelling the covariance structure in
#' marginal multivariate count models: Hunting in Bioko Island.
#' Environmetrics, submitted.
#'
#' @seealso \code{mc_sic_covariance}.
#'
#' @return A data frame containing SIC values, degree of freedom,
#' Tu-statistics and chi-squared reference values.
#'
#' @examples
#' set.seed(123)
#' x1 <- runif(100, -1, 1)
#' x2 <- gl(2,50)
#' beta = c(5, 0, 3)
#' X <- model.matrix(~ x1 + x2)
#' y <- rnorm(100, mean = X%*%beta , sd = 1)
#' data <- data.frame(y, x1, x2)
#' # Reference model
#' fit0 <- mcglm(c(y ~ 1), list(mc_id(data)), data = data)
#' # Computing SIC
#' mc_sic(fit0, scope = c("x1","x2"), data = data, response = 1)
#'
#' @export

mc_sic <- function(object, scope, data, response, penalty = 2) {
    SIC <- c()
    df <- c()
    df_total <- c()
    TU <- c()
    QQ <- c()
    for (i in 1:length(scope)) {
        ini_formula <- object$linear_pred[[response]]
        ext_formula <- as.formula(
            paste("~", paste(ini_formula[3], scope[i], sep = "+")))
        md <- model.frame(object$linear_pred[[response]], data = data)
        Y <- model.response(md)
        ini_beta <- coef(object, type = "beta",
                         response = response)$Estimates
        ext_X <- model.matrix(ext_formula, data = data)
        n_beta <- dim(ext_X)[2]
        n_ini_beta <- length(ini_beta)
        ext_beta <- c(ini_beta, rep(0, n_beta - n_ini_beta))
        n_total_beta <- length(ext_beta)
        mu_temp <- mc_link_function(beta = ext_beta, X = ext_X,
                                    offset = NULL,
                                    link = object$link[[response]])
        score_temp <- mc_quasi_score(D = mu_temp$D,
                                     inv_C = object$inv_C, y_vec = Y,
                                     mu_vec = mu_temp$mu)
        S11 <- score_temp$Variability[1:n_ini_beta, 1:n_ini_beta]
        S22 <- score_temp$Variability[c(n_ini_beta + 1):n_total_beta,
                                      c(n_ini_beta + 1):n_total_beta]
        S12 <- score_temp$Variability[1:n_ini_beta,
                                      c(n_ini_beta + 1):n_total_beta]
        S21 <- score_temp$Variability[c(n_ini_beta + 1):n_total_beta,
                                      1:n_ini_beta]
        VB <- S22 - S21 %*% solve(S11) %*% S12
        Tu <- t(score_temp$Score[c(n_ini_beta + 1):n_total_beta]) %*%
            solve(VB) %*%
            score_temp$Score[c(n_ini_beta + 1):n_total_beta]
        df[i] <- n_beta - n_ini_beta
        SIC[i] <- -as.numeric(Tu) + penalty * n_beta
        df_total[i] <- n_beta
        TU[i] <- as.numeric(Tu)
        QQ[i] <- qchisq(0.95, df = df[i])
    }
    output <- data.frame(SIC = SIC, Covariates = scope, df = df,
                         df_total = df_total, Tu = TU, Chisq = QQ)
    return(output)
}

#' @title Variance Functions
#' @author Wagner Hugo Bonat, \email{wbonat@@ufpr.br}
#'
#' @description Compute the variance function and its derivatives with
#' respect to regression, dispersion and power parameters.
#'
#' @param mu a numeric vector. In general the output from
#'     \code{\link{mc_link_function}}.
#' @param power a numeric value (\code{power} and \code{binomialP}) or
#'     a vector (\code{binomialPQ}) of the power parameters.
#' @param Ntrial number of trials, useful only when dealing with
#'     binomial response variables.
#' @param variance a string specifying the name (\code{power, binomialP
#'     or binomialPQ}) of the variance function.
#' @param inverse logical. Compute the inverse or not.
#' @param derivative_power logical if compute (TRUE) or not (FALSE) the
#'     derivatives with respect to the power parameter.
#' @param derivative_mu logical if compute (TRUE) or not (FALSE) the
#'     derivative with respect to the mu parameter.
#' @return A list with from one to four elements depends on the
#'     arguments.
#'
#' @seealso \code{\link{mc_link_function}}.
#'
#' @details The function \code{mc_variance_function} computes three
#'     features related with the variance function. Depending on the
#'     logical arguments, the function returns \eqn{V^{1/2}} and its
#'     derivatives with respect to the parameters power and mu,
#'     respectivelly.  The output is a named list, completely
#'     informative about what the function has been computed.  For
#'     example, if \code{inverse = FALSE}, \code{derivative_power =
#'     TRUE} and \code{derivative_mu = TRUE}. The output will be a list,
#'     with three elements: V_sqrt, D_V_sqrt_power and D_V_sqrt_mu.
#'
#' @usage mc_variance_function(mu, power, Ntrial, variance, inverse,
#'                            derivative_power, derivative_mu)
#'
#' @source Bonat, W. H. and Jorgensen, B. (2016) Multivariate
#'     covariance generalized linear models.
#'     Journal of Royal Statistical Society - Series C 65:649--675.
#'
#' @export
#'
#' @examples
#' x1 <- seq(-1, 1, l = 5)
#' X <- model.matrix(~x1)
#' mu <- mc_link_function(beta = c(1, 0.5), X = X, offset = NULL,
#'                        link = "logit")
#' mc_variance_function(mu = mu$mu, power = c(2, 1), Ntrial = 1,
#'                      variance = "binomialPQ", inverse = FALSE,
#'                      derivative_power = TRUE, derivative_mu = TRUE)
#'

## Generic variance function -------------------------------------------
mc_variance_function <- function(mu, power, Ntrial,
                                 variance, inverse,
                                 derivative_power,
                                 derivative_mu) {
    assert_that(is.logical(inverse))
    assert_that(is.logical(derivative_power))
    assert_that(is.logical(derivative_mu))
    switch(variance,
           power = {
               output <- mc_power(mu = mu, power = power,
                                  inverse = inverse,
                                  derivative_power = derivative_power,
                                  derivative_mu = derivative_mu)
           },
           binomialP = {
               output <- mc_binomialP(mu = mu, power = power,
                                      Ntrial = Ntrial,
                                      inverse = inverse,
                                      derivative_power =
                                          derivative_power,
                                      derivative_mu = derivative_mu)
           },
           binomialPQ = {
               output <- mc_binomialPQ(mu = mu, power = power,
                                       Ntrial = Ntrial,
                                       inverse = inverse,
                                       derivative_power =
                                           derivative_power,
                                       derivative_mu = derivative_mu)
           },
           stop(gettextf("%s variance function not recognised",
                         sQuote(variance)), domain = NA))
    return(output)
}

#' @rdname mc_variance_function
## Power variance function ---------------------------------------------
mc_power <- function(mu, power, inverse,
                     derivative_power,
                     derivative_mu) {
    ## The observed value can be zero, but not the expected value.
    assert_that(all(mu > 0))
    assert_that(is.number(power))
    mu.power <- mu^power
    sqrt.mu.power <- sqrt(mu.power)
    n <- length(mu)
    if (inverse == TRUE & derivative_power == TRUE &
            derivative_mu == FALSE) {
        output <- list(
            V_inv_sqrt = Diagonal(n = n, 1/sqrt.mu.power),
            D_V_inv_sqrt_power =
                Diagonal(n = n,
                         -(mu.power * log(mu))/(2 * (mu.power)^(1.5))))
    }
    if (inverse == TRUE & derivative_power == FALSE &
            derivative_mu == FALSE) {
        output <- list(V_inv_sqrt = Diagonal(n = n, 1/sqrt.mu.power))
    }
    if (inverse == FALSE & derivative_power == TRUE &
            derivative_mu == FALSE) {
        output <- list(
            V_sqrt = Diagonal(n = n, sqrt.mu.power),
            D_V_sqrt_power =
                Diagonal(n = n,
                         +(mu.power * log(mu))/(2 * sqrt.mu.power)))
    }
    if (inverse == FALSE & derivative_power == FALSE &
            derivative_mu == FALSE) {
        output <- list(V_sqrt = Diagonal(n = n, sqrt.mu.power))
    }
    if (inverse == TRUE & derivative_power == TRUE &
            derivative_mu == TRUE) {
        output <- list(
            V_inv_sqrt = Diagonal(n = n, 1/sqrt.mu.power),
            D_V_inv_sqrt_power =
                Diagonal(n = n,
                         -(mu.power * log(mu))/(2 * (mu.power)^(1.5))),
            D_V_inv_sqrt_mu = -(mu^(power -  1) * power)/
                                   (2 * (mu.power)^(1.5)))
    }
    if (inverse == TRUE & derivative_power == FALSE &
            derivative_mu == TRUE) {
        output <- list(
            V_inv_sqrt = Diagonal(n = n, 1/sqrt.mu.power),
            D_V_inv_sqrt_mu = -(mu^(power - 1) * power)/
                                   (2 * (mu.power)^(1.5)))
    }
    if (inverse == FALSE & derivative_power == TRUE &
            derivative_mu == TRUE) {
        output <- list(
            V_sqrt = Diagonal(n = n, sqrt.mu.power),
            D_V_sqrt_power =
                Diagonal(n = n, (mu.power * log(mu))/
                                    (2 * sqrt.mu.power)),
            D_V_sqrt_mu = (mu^(power - 1) * power)/(2 * sqrt.mu.power))
    }
    if (inverse == FALSE & derivative_power == FALSE &
            derivative_mu == TRUE) {
        output <- list(V_sqrt = Diagonal(n = n, sqrt.mu.power),
                       D_V_sqrt_mu = (mu^(power - 1) * power)/
                                         (2 * sqrt.mu.power))
    }
    return(output)
}

#' @rdname mc_variance_function
#' @usage mc_binomialP(mu, power, inverse, Ntrial,
#'                     derivative_power, derivative_mu)
## BinomialP variance function
## -----------------------------------------
mc_binomialP <- function(mu, power, inverse, Ntrial,
                         derivative_power,
                         derivative_mu) {
    ## The observed value can be 0 and 1, but not the expected value
    assert_that(all(mu > 0))
    assert_that(all(mu < 1))
    assert_that(is.number(power))
    assert_that(all(Ntrial > 0))
    constant <- (1/Ntrial)
    mu.power <- mu^power
    mu.power1 <- (1 - mu)^power
    mu1mu <- constant * (mu.power * mu.power1)
    sqrt.mu1mu <- sqrt(mu1mu)
    n <- length(mu)
    if (inverse == TRUE & derivative_power == TRUE &
            derivative_mu == FALSE) {
        output <- list(
            V_inv_sqrt = Diagonal(n = n, 1/sqrt.mu1mu),
            D_V_inv_sqrt_power =
                Diagonal(n = n, -(log(1 - mu) * mu1mu +
                                  log(mu) * mu1mu)/(2 * (mu1mu^(1.5)))))
    }
    if (inverse == TRUE & derivative_power == FALSE &
            derivative_mu == FALSE) {
        output <- list(V_inv_sqrt = Diagonal(n = n, 1/sqrt.mu1mu))
    }
    if (inverse == FALSE & derivative_power == TRUE &
            derivative_mu == FALSE) {
        output <- list(
            V_sqrt = Diagonal(n = n, sqrt.mu1mu),
            D_V_sqrt_power = Diagonal(n = n, (log(1 - mu) * mu1mu +
                                              log(mu) * mu1mu)/
                                                 (2 * sqrt.mu1mu)))
    }
    if (inverse == FALSE & derivative_power == FALSE &
            derivative_mu == FALSE) {
        output <- list(V_sqrt = Diagonal(n = n, sqrt.mu1mu))
    }
    if (inverse == TRUE & derivative_power == TRUE &
            derivative_mu == TRUE) {
        output <- list(
            V_inv_sqrt = Diagonal(n = n, 1/sqrt.mu1mu),
            D_V_inv_sqrt_power =
                Diagonal(n = n, -(log(1 - mu) * mu1mu + log(mu) *
                                  mu1mu)/(2 * (mu1mu^(1.5)))),
            D_V_inv_sqrt_mu = -(constant * (mu.power1 *
                                            (mu^(power - 1)) * power) -
                                constant * (((1 - mu)^(power - 1)) *
                                            mu.power * power))/
                                   (2 * (mu1mu^(1.5))))
    }
    if (inverse == TRUE & derivative_power == FALSE &
            derivative_mu == TRUE) {
        output <- list(
            V_inv_sqrt = Diagonal(n = n, 1/sqrt.mu1mu),
            D_V_inv_sqrt_mu = -(constant *
                                (mu.power1 * (mu^(power - 1)) * power) -
                                constant * (((1 - mu)^(power - 1)) *
                                            mu.power * power))/
                                   (2 * (mu1mu^(1.5))))
    }
    if (inverse == FALSE & derivative_power == TRUE &
            derivative_mu == TRUE) {
        output <- list(
            V_sqrt = Diagonal(n = n, sqrt.mu1mu),
            D_V_sqrt_power = Diagonal(n = n, (log(1 - mu) * mu1mu +
                                              log(mu) * mu1mu)/
                                                 (2 * sqrt.mu1mu)),
            D_V_sqrt_mu = (constant *
                           (mu.power1 * (mu^(power - 1)) * power) -
                           constant * (((1 - mu)^(power - 1)) *
                                       mu.power * power))/
                              (2 * sqrt.mu1mu))
    }
    if (inverse == FALSE & derivative_power == FALSE &
            derivative_mu == TRUE) {
        output <- list(
            V_sqrt = Diagonal(n = n, sqrt.mu1mu),
            D_V_sqrt_mu = (constant *
                           (mu.power1 * (mu^(power - 1)) * power) -
                           constant * (((1 - mu)^(power - 1)) *
                                       mu.power * power))/
                              (2 * sqrt.mu1mu))
    }
    return(output)
}

#' @rdname mc_variance_function
#' @usage mc_binomialPQ(mu, power, inverse, Ntrial,
#'                      derivative_power, derivative_mu)
## BinomialPQ variance function ----------------------------------------
mc_binomialPQ <- function(mu, power, inverse,
                          Ntrial, derivative_power,
                          derivative_mu) {
    ## The observed value can be 0 and 1, but not the expected value
    assert_that(all(mu > 0))
    assert_that(all(mu < 1))
    assert_that(length(power) == 2)
    assert_that(all(Ntrial > 0))
    constant <- (1/Ntrial)
    p <- power[1]
    q <- power[2]
    mu.p <- mu^p
    mu1.q <- (1 - mu)^q
    mu.p.mu.q <- mu.p * mu1.q
    mu1mu <- mu.p.mu.q * constant
    sqrt.mu1mu <- sqrt(mu1mu)
    n <- length(mu)
    if (inverse == TRUE & derivative_power == TRUE &
            derivative_mu == FALSE) {
        denominator <- (2 * (mu1mu^1.5) * Ntrial)
        output <- list(
            V_inv_sqrt = Diagonal(n = n, 1/sqrt.mu1mu),
            D_V_inv_sqrt_p = Diagonal(n = n,
                                      -(mu.p.mu.q * log(mu))/
                                           denominator),
            D_V_inv_sqrt_q = Diagonal(n = n,
                                      -mu.p.mu.q * log(1 - mu)/
                                           denominator))
    }
    if (inverse == TRUE & derivative_power == FALSE &
            derivative_mu == FALSE) {
        output <- list(V_inv_sqrt = Diagonal(n = n, 1/sqrt.mu1mu))
    }
    if (inverse == FALSE & derivative_power == TRUE &
            derivative_mu == FALSE) {
        denominator <- 2 * sqrt.mu1mu * Ntrial
        output <- list(
            V_sqrt = Diagonal(n = n, sqrt.mu1mu),
            D_V_sqrt_p = Diagonal(n = n,
                                  +(mu.p.mu.q * log(mu))/denominator),
            D_V_sqrt_q = Diagonal(n = n,
                                  +(mu.p.mu.q * log(1 - mu))/
                                       denominator))
    }
    if (inverse == FALSE & derivative_power == FALSE &
            derivative_mu == FALSE) {
        output <- list(V_sqrt = Diagonal(n = n, sqrt.mu1mu))
    }
    if (inverse == TRUE & derivative_power == TRUE &
            derivative_mu == TRUE) {
        denominator <- (2 * (mu1mu^1.5) * Ntrial)
        output <- list(
            V_inv_sqrt = Diagonal(n = n, 1/sqrt.mu1mu),
            D_V_inv_sqrt_p = Diagonal(n = n,
                                      -(mu.p.mu.q * log(mu))/
                                           denominator),
            D_V_inv_sqrt_q = Diagonal(n = n,
                                      -mu.p.mu.q *
                                           log(1 - mu)/denominator),
            D_V_inv_sqrt_mu = -(constant *
                                (mu1.q * (mu^(p - 1)) * p) -
                                constant * (((1 - mu)^(q - 1)) *
                                            mu.p * q))/
                                   (2 * (mu1mu^1.5)))
    }
    if (inverse == TRUE & derivative_power == FALSE &
            derivative_mu == TRUE) {
        output <- list(
            V_inv_sqrt = Diagonal(n = n, 1/sqrt.mu1mu),
            D_V_inv_sqrt_mu = -(constant * (mu1.q * (mu^(p - 1)) * p) -
                                constant * (((1 - mu)^(q - 1)) *
                                            mu.p * q))/
                                   (2 * (mu1mu^1.5)))
    }
    if (inverse == FALSE & derivative_power == TRUE &
            derivative_mu == TRUE) {
        denominator1 <- 2 * sqrt.mu1mu
        denominator2 <- denominator1 * Ntrial
        output <- list(
            V_sqrt = Diagonal(n = n, sqrt.mu1mu),
            D_V_sqrt_p = Diagonal(n = n, (mu.p.mu.q * log(mu))/
                                             denominator2),
            D_V_sqrt_q = Diagonal(n = n, (mu.p.mu.q * log(1 - mu))/
                                             denominator2),
            D_V_sqrt_mu = (constant * (mu1.q * (mu^(p - 1)) * p) -
                           constant * (((1 - mu)^(q - 1)) * mu.p * q))/
                              denominator1)
    }
    if (inverse == FALSE & derivative_power == FALSE &
            derivative_mu == TRUE) {
        output <- list(
            V_sqrt = Diagonal(n = n, sqrt.mu1mu),
            D_V_sqrt_mu = (constant * (mu1.q * (mu^(p - 1)) * p) -
                           constant * (((1 - mu)^(q - 1)) * mu.p * q))/
                              (2 * sqrt.mu1mu))
    }
    return(output)
}

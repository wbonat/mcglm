#' Compute quasi-likelihood function.
#'
#' Given a variance function mc_qll function computes the quasi-likelihood values.
#' @param y A vector of observed values.
#' @param mu A vector of fitted values.
#' @param variance Variance function (constant, tweedie, poisson_tweedie, binomial).
#' @param power Power parameter value.
#' @return The quasi-likelihood values.
#' @export

mc_qll <- function(y, mu, variance, power) {
    if (variance == "constant") {
        qll <- -((y - mu)^2)/2  # Gaussian case
    }
    if (variance == "tweedie" & power == 1) {
        qll <- y * log(mu) - mu  # Poisson case
    }
    if (variance == "tweedie" & power == 2) {
        -y/mu - log(mu)  # Gamma case
    }
    if (variance == "tweedie" & power != 1 & power != 2) {
        qll <- (mu^-power) * ((mu * y)/(1 - power) - (mu^2)/(2 - power))  # General Tweedie case
    }
    if (variance == "poisson_tweedie" & power == 1) {
        qll <- (y * log(mu) - mu) + (y * log(mu) - mu)
    }
    if (variance == "poisson_tweedie" & power == 2) {
        qll <- (y * log(mu) - mu) + (-y/mu - log(mu))
    }
    if (variance == "poisson_tweedie" & power != 1 & power != 2) {
        qll <- (y * log(mu) - mu) + (mu^-power) * ((mu * y)/(1 - power) - (mu^2)/(2 - power))
    }
    if (variance == "binomial") {
        qll <- y * log(mu/(1 - mu)) + log(1 - mu)  # Binomial case
    }
    return(qll)
}

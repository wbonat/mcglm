#' @title Build the correlation matrix between response variables
#' @author Wagner Hugo Bonat
#'
#' @description This function builds the correlation matrix between
#'     response variable, its inverse and derivatives.
#'
#' @param rho A numeric vector.
#' @param n_resp A numeric.
#' @param inverse Logical.
#' @keywords internal
#' @return A list with sigmab and its derivatives with respect to rho.

mc_build_sigma_between <- function(rho, n_resp, inverse = FALSE) {
    output <- list(Sigmab = 1, D_Sigmab = 1)
    if (n_resp > 1) {
        Sigmab <- Diagonal(n_resp, 1)
        Sigmab[lower.tri(Sigmab)] <- rho
        Sigmab <- forceSymmetric(t(Sigmab))
        D_Sigmab <- mc_derivative_sigma_between(n_resp = n_resp)
        if (inverse == FALSE) {
            output <- list(Sigmab = Sigmab, D_Sigmab = D_Sigmab)
        }
        if (inverse == TRUE) {
            inv_Sigmab <- solve(Sigmab)
            D_inv_Sigmab <- lapply(D_Sigmab, mc_sandwich_negative,
                                   bord1 = inv_Sigmab,
                                   bord2 = inv_Sigmab)
            output <- list(inv_Sigmab = inv_Sigmab,
                           D_inv_Sigmab = D_inv_Sigmab)
        }
    }
    return(output)
}

#' @rdname mc_build_sigma_between
mc_derivative_sigma_between <- function(n_resp) {
    position <- combn(n_resp, 2)
    list.Derivative <- list()
    n_par <- n_resp * (n_resp - 1)/2
    for (i in 1:n_par) {
        Derivative <- Matrix(0, ncol = n_resp, nrow = n_resp)
        Derivative[position[1, i], position[2, i]] <-
            Derivative[position[2, i], position[1, i]] <- 1
        list.Derivative[i][[1]] <- Derivative
    }
    return(list.Derivative)
}

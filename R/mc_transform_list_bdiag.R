#' @title Auxiliar function to compute the derivatives of the C matrix.
#' @author Wagner Hugo Bonat
#'
#' @description This function take a list of matrices and return a list
#'     of block-diagonal matrices, where the original matrices are one
#'     block non-zero of the matrix.
#'
#' @param list_mat A list of matrices.
#' @param mat_zero A list of zero matrices. In general the output of
#'     \code{link[mcglm]{mc_build_bdiag}}.
#' @param response_number A numeric specifying the response variable
#'     number.
#' @keywords internal
#' @return A list of block-diagonal matrices.

mc_transform_list_bdiag <- function(list_mat, mat_zero,
                                    response_number) {
    aux.f <- function(x, mat_zero, response_number) {
        mat_zero[[response_number]] <- x
        return(bdiag(mat_zero))
    }
    output <- lapply(list_mat, aux.f, mat_zero = mat_zero,
                     response_number = response_number)
    return(output)
}

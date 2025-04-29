

#' @title Convert \link[base]{isSymmetric} Matrix to \link[flextable]{flextable}
#' 
#' @description
#' To convert an \link[base]{isSymmetric} matrix to a \link[flextable]{flextable}.
#' 
#' @param x \link[base]{isSymmetric} \link[base]{matrix}
#' 
#' @param ... additional parameters of function \link[base]{format.default}
#' 
#' @returns
#' Function [symmetric2flextable()] returns a \link[flextable]{flextable}.
#' 
#' @keywords internal
#' @seealso \link[stats]{cov2cor}
#' @export
symmetric2flextable <- function(x, ...) {
  
  if (anyNA(x)) stop('do not allow missing')
  if (!isSymmetric.matrix(x)) stop('input matrix must be symmetric')
  
  ret <- format.default(x, ...)
  d <- dim(x)
  
  if (all(diag(x) == 1)) {
    # e.g., a \link[stats]{cor}relation matrix
    ret[.row(d) <= .col(d)] <- ''
    return(as_flextable.matrix(ret[-1L, -d[1L], drop = FALSE]))
  } 
  
  ret[.row(d) < .col(d)] <- ''
  return(as_flextable.matrix(ret))
  
}




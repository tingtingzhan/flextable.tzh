
#' @title format4flextable
#' 
#' @param x \link[base]{data.frame}
#' 
#' @returns 
#' Function [format4flextable()] returns a \link[base]{data.frame}
#' 
#' @keywords internal
#' @export
format4flextable <- function(x) {
  x[] <- lapply(x, FUN = \(ix) {
    if (inherits(ix, what = 'Surv')) {
      format(ix) 
      # ?survival::format.Surv # convert 'Surv' (inherits from 'matrix') to 'character'
    } else ix
  })
  return(x)
}

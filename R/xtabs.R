

#' @title Convert \link[stats]{xtabs} to \link[flextable]{flextable}
#' 
#' @description
#' ..
#' 
#' @param x an \link[stats]{xtabs} object
#' 
#' @param ... additional parameters of function [as_flextable.array]
#' 
#' @details
#' Prints zero-counts as `'.'`.
#' 
#' @note
#' Without this S3 dispatch, will be dispatched to `flextable:::as_flextable.table`.
#' 
#' @examples
#' # ?stats::xtabs
#' typeof(UCBAdmissions) # 'double'
#' (m = xtabs(Freq ~ Gender + Admit, data = as.data.frame(UCBAdmissions)))
#' typeof(m) # double
#' flextable:::as_flextable.table(m)
#' as_flextable(m)
#' @export as_flextable.xtabs
#' @export
as_flextable.xtabs <- function(x, ...) {
  # stopifnot(typeof(x) == 'integer') # No!! could be 'double'
  y <- x
  storage.mode(y) <- 'character'
  y[x == 0L] <- '.'
  as_flextable.array(y, ...)
}

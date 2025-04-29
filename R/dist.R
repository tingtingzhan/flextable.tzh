

#' @title Convert \link[stats]{dist} into \link[flextable]{flextable}
#' 
#' @description ..
#' 
#' @param x \link[stats]{dist} object
#' 
#' @param ... additional parameters of function [as_flextable.matrix()]
#' 
#' @returns 
#' Function [as_flextable.dist()] returns a \link[flextable]{flextable}.
#' 
#' @keywords internal
#' @importFrom flextable as_flextable
#' @export as_flextable.dist
#' @export
as_flextable.dist <- function(x, ...) {
  # do not overwrite ?stats:::format.dist
  # inspired by ?stats:::print.dist
  Labels <- attr(x, which = 'Labels', exact = TRUE)
  Size <- attr(x, which = 'Size', exact = TRUE)
  if (!length(Labels)) Labels <- sprintf(fmt = '[%d]', seq_len(Size))
  .dim <- c(Size, Size)
  ret <- array('', dim = .dim, dimnames = list(Labels, Labels))
  lo <- .row(.dim) - .col(.dim) # ?base::lower.tri
  ret[lo > 0L] <- format(x, ...) # ?stats:::format.dist
  as_flextable.matrix(ret[-1L, -Size, drop = FALSE], ...)
}


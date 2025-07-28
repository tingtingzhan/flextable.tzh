
#' @title Adjusted \eqn{p}-Values in All Correction Methods
#' 
#' @description 
#' Adjusted \eqn{p}-values in all correction methods.
#' 
#' @param x see **Usage**
#' 
#' @returns
#' All method dispatches of generic function [p_adjust_] return a \link[base]{matrix} of adjusted \eqn{p}-values using all available \link[stats]{p.adjust.methods}.
#' 
#' @keywords internal
#' @name p_adjust_
#' @export
p_adjust_ <- function(x) UseMethod(generic = 'p_adjust_')


#' @rdname p_adjust_
#' @importFrom stats p.adjust p.adjust.methods
#' @export p_adjust_.numeric
#' @export
p_adjust_.numeric <- function(x) {
  
  method <- setdiff(p.adjust.methods, y = c('fdr', if (sum(!is.na(x)) == 2L) 'hommel'))
  # 'fdr' is 'BH'
  # 'hommel' for n == 2L is 'hochberg'
  
  ret <- method |>
    lapply(FUN = p.adjust, p = x) |>
    do.call(what = cbind) |>
    pmin(1)
  colnames(ret) <- method
  class(ret) <- 'p_adjust'
  return(ret)
  
}






#' @rdname p_adjust_
#  a `'pairwise.htest'` object, as returned from functions \link[stats]{pairwise.t.test},
# \link[stats]{pairwise.wilcox.test} or
# \link[stats]{pairwise.prop.test}.
#' 
#' @method p_adjust_ pairwise.htest
#' @export p_adjust_.pairwise.htest
#' @export
p_adjust_.pairwise.htest <- function(x) {
  
  if (x$p.adjust.method != 'none') stop('input must use `p.adjust.method = \'none\'`')
  if (!is.matrix(pv0 <- x$p.value)) stop('input must have matrix `$p.value`')
  
  id <- lower.tri(pv0, diag = TRUE)
  pv <- pv0[id]
  
  dnm <- dimnames(pv0)
  names(pv) <- outer(dnm[[1L]], dnm[[2L]], FUN = paste, sep = ' vs. ')[id]
  
  ret <- p_adjust_.numeric(pv) # 'matrix'
  names(dimnames(ret)) <- c(x$method, '') # for ?as_flextable.matrix
  return(ret)
  
}



#' @title [as_flextable.p_adjust]
#' 
#' @param x `p_adjust` object
#' 
#' @param ... ..
#' 
#' @keywords internal
#' @importFrom flextable as_flextable
#' @importFrom rmd.tzh label_pvalue_sym
#' @export as_flextable.p_adjust
#' @export
as_flextable.p_adjust <- function(x, ...) {
  x |>
    unclass() |> # back to 'matrix'
    label_pvalue_sym()() |> 
    as_flextable.matrix()
}




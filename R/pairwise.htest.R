
#' @title Convert \eqn{p}-values of `pairwise.htest` Object into \link[flextable]{flextable}
#' 
#' @description
#' Convert \eqn{p}-values of a `pairwise.htest` object into a \link[flextable]{flextable}.
#' 
#' @param x a `'pairwise.htest'` object, as returned from functions \link[stats]{pairwise.t.test},
#' \link[stats]{pairwise.wilcox.test} or
#' \link[stats]{pairwise.prop.test}.
#' 
#' @param row.title,... additional parameters of function [as_flextable.matrix()].
#' 
#' @returns
#' Function [as_flextable.pairwise.htest] returns a \link[flextable]{flextable}.
#' 
#' @keywords internal
#' @importFrom flextable as_flextable
#' @importFrom rmd.tzh label_pvalue_sym
#' @method as_flextable pairwise.htest
#' @export as_flextable.pairwise.htest
#' @export
as_flextable.pairwise.htest <- function(x, row.title = x$method, ...) {
  x$p.value |>
    label_pvalue_sym()() |>
    as_flextable.matrix(row.title = row.title, ...)
}


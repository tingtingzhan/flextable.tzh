
#' @title Convert \eqn{p}-values of `pairwise.htest` Object into \link[flextable]{flextable}
#' 
#' @description
#' Convert \eqn{p}-values of a `pairwise.htest` object into a \link[flextable]{flextable}.
#' 
#' @param x a `'pairwise.htest'` object, as returned from functions \link[stats]{pairwise.t.test},
#' \link[stats]{pairwise.wilcox.test} or
#' \link[stats]{pairwise.prop.test}.
#' 
#' @param row.title,... additional parameters of function [as_flextable.array]
#' 
#' @returns
#' Function [as_flextable.pairwise.htest] returns a \link[flextable]{flextable}.
#' 
#' @examples
#' airquality |> 
#'   within.data.frame(expr = {
#'     Month = factor(Month, labels = month.abb[5:9])
#'   }) |>
#'   with(expr = pairwise.t.test(Ozone, Month, pool.sd = FALSE, p.adj = 'none')) |>
#'   as_flextable()
#' @method as_flextable pairwise.htest
#' @export as_flextable.pairwise.htest
#' @export
as_flextable.pairwise.htest <- function(x, row.title = x$method, ...) {
  as_flextable.array(format_pval(x$p.value), row.title = row.title, ...)
}

#' @title as_flextable.pairwise.htest
#' 
#' @param x a `'pairwise.htest'` object, as returned from \link[stats]{pairwise.t.test},
#' \link[stats]{pairwise.wilcox.test} or
#' \link[stats]{pairwise.prop.test}.
#' 
#' @param row.title,... additional parameters of [as_flextable.array]
#' 
#' @examples
#' aq = within(airquality, expr = {
#'   Month = factor(Month, labels = month.abb[5:9])
#' })
#' (x = with(aq, pairwise.t.test(Ozone, Month, pool.sd = FALSE, p.adj = 'none')))
#' as_flextable(x)
#' @method as_flextable pairwise.htest
#' @export as_flextable.pairwise.htest
#' @export
as_flextable.pairwise.htest <- function(x, row.title = x$method, ...) {
  as_flextable.array(format_pval(x$p.value), row.title = row.title, ...)
}
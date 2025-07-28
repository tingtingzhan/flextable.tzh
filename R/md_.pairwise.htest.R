

#' @title Markdown Lines for `pairwise.htest` object
#' 
#' @param x `pairwise.htest` object
#' 
#' @param xnm ..
#' 
#' @param ... ..
#' 
#' @examples
#' library(rmd.tzh); list(
#'   '`pairwise.htest`' = airquality |> 
#'     within.data.frame(expr = {
#'       Month = factor(Month, labels = month.abb[5:9])
#'     }) |>
#'     with(expr = pairwise.t.test(Ozone, Month, pool.sd = FALSE, p.adj = 'none'))
#' ) |> render_(file = 'pairwise.htest')
#' 
#' @importFrom rmd.tzh md_
#' @importClassesFrom rmd.tzh md_lines
#' @export md_.pairwise.htest
#' @export
md_.pairwise.htest <- function(x, xnm, ...) {
  c(
    '```{r}', # multiple ?flextable::flextable
    '#| echo: false', 
    sprintf(fmt = '(%s) |> as_flextable.pairwise.htest()', xnm), 
    sprintf(fmt = '(%s) |> p_adjust_.pairwise.htest() |> label_pvalue_sym()() |> as_flextable.matrix()', xnm), 
    '```'
  ) |> 
    new(Class = 'md_lines')
}


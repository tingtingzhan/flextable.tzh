
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
#' @keywords internal
#' @importFrom utils bibentry
#' @importFrom rmd.tzh md_
#' @importClassesFrom rmd.tzh md_lines
#' @export md_.pairwise.htest
#' @export
md_.pairwise.htest <- function(x, xnm, ...) {
  
  c(
    
    x$method |> 
      sprintf(fmt = 'Pairwise %s are performed using <u>**`R`**</u>.'),
    '```{r}',
    '#| echo: false', 
    sprintf(fmt = '(%s) |> as_flextable.pairwise.htest()', xnm), 
    '```',
    
    'Adjusted $p$-values [@Holm79; @Hochberg88; @Hommel88; @BenjaminiHochberg95; @BenjaminiYekutieli01] for multiple comparison are provided as well.',
    '```{r}',
    '#| echo: false', 
    sprintf(fmt = '(%s) |> p_adjust_.pairwise.htest() |> as_flextable.p_adjust()', xnm), 
    '```'
    
  ) |> 
    new(Class = 'md_lines', bibentry = c(
      bibentry(bibtype = 'Article', key = 'Holm79',
        url = 'http://www.jstor.org/stable/4615733',
        author = 'Sture Holm',
        journal = 'Scandinavian Journal of Statistics',
        number = '2',
        pages = '65--70',
        title = 'A Simple Sequentially Rejective Multiple Test Procedure',
        volume = '6',
        year = '1979'
      ),
      bibentry(bibtype = 'Article', key = 'Hochberg88',
        author = 'Yosef Hochberg',
        title = 'A sharper {B}onferroni procedure for multiple tests of significance',
        journal = 'Biometrika',
        volume = '75',
        number = '4',
        pages = '800-802',
        year = '1988',
        month = '12',
        doi = '10.1093/biomet/75.4.800'
      ),
      bibentry(bibtype = 'Article', key = 'Hommel88',
        author = 'Gerhard Hommel',
        title = 'A stagewise rejective multiple test procedure based on a modified Bonferroni test',
        journal = 'Biometrika',
        volume = '75',
        number = '2',
        pages = '383-386',
        year = '1988',
        month = '06',
        doi = '10.1093/biomet/75.2.383'
      ),
      bibentry(bibtype = 'Article', key = 'BenjaminiHochberg95',
        author = 'Yoav Benjamini and Yosef Hochberg',
        title = 'Controlling the False Discovery Rate: A Practical and Powerful Approach to Multiple Testing',
        journal = 'Journal of the Royal Statistical Society: Series B (Methodological)',
        volume = '57',
        number = '1',
        pages = '289-300',
        year = '1995',
        month = '12',
        doi = '10.1111/j.2517-6161.1995.tb02031.x'
      ),
      bibentry(bibtype = 'Article', key = 'BenjaminiYekutieli01',
        author = 'Yoav Benjamini and Daniel Yekutieli',
        title = 'The control of the false discovery rate in multiple testing under dependency',
        volume = '29',
        journal = 'The Annals of Statistics',
        number = '4',
        pages = '1165 -- 1188',
        year = '2001',
        doi = '10.1214/aos/1013699998'
      )
    ))
  
  # multiple ?flextable::flextable can be put in one code-chunk :)
}


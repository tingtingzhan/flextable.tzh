

#' @title Convert \link[MatchIt]{matchit} to \link[flextable]{flextable}
#' 
#' @param x a \link[MatchIt]{matchit} object
#' 
#' @param ... additional parameters of function `MatchIt:::summary.matchit`
#' 
#' @returns 
#' Functions [as_flextable.matchit()] and [as_flextable.summary.matchit()] both return a \link[flextable]{flextable}.
#' 
#' @name flextable_matchit
#' @keywords internal
#' @importFrom flextable as_flextable
#' @export as_flextable.matchit
#' @export
as_flextable.matchit <- function(x, ...) {
  x |> 
    summary(...) |> # ?MatchIt:::summary.matchit
    as_flextable.summary.matchit()
  # I still do not know where to draw the [hline] to separate `mv` and `addlvariables` 
  # 'factor's in `addlvariables` could also be expanded in multiple rows..
}

#' @rdname flextable_matchit
#' @importFrom flextable as_flextable flextable colformat_double vline autofit
#' @export as_flextable.summary.matchit
#' @export
as_flextable.summary.matchit <- function(x, ...) {
  mv <- all.vars(x$call$formula[[3L]]) # covariates to be matched; (m)atched-co(v)ariates
  
  z <- x$sum.matched |> # 'matrix'
    as.data.frame.matrix(make.names = FALSE)
  
  data.frame(
    ' ' = rownames(z),
    z,
    check.names = FALSE
  ) |>
    flextable() |>
    colformat_double(digits = 3L) |>
    vline(j = 1L) |>
    autofit(part = 'all')
  
}




#' @title R Markdown Lines for \link[MatchIt]{matchit}
#' 
#' @param x,xnm,... ..
#' 
#' @examples
#' library(MatchIt)
#' m = matchit(treat ~ age+educ+race+nodegree+married+re74+re75, data = lalonde)
#' list(
#'  '`matchit1`' = m,
#'  '`matchit2`' = m |> summary(addlvariables = 're78')
#' ) |> rmd.tzh::render_(file = 'matchit')
#' @keywords internal
#' @name md_matchit
#' @importFrom rmd.tzh md_
#' @export md_.matchit
#' @export
md_.matchit <- function(x, xnm, ...) {
  md_.summary.matchit(
    x = summary(x), # ?MatchIt:::summary.matchit
    xnm = sprintf(fmt = 'summary(%s)', xnm),
    ...)
}





#' @rdname md_matchit
#' @importFrom stats formula
#' @importFrom utils bibentry
#' @importFrom rmd.tzh md_
#' @importClassesFrom rmd.tzh md_lines
#' @export md_.summary.matchit
#' @export
md_.summary.matchit <- function(x, xnm, ...) {
  
  n_all <- x$nn['All', ] # stop if error
  n_matched <- x$nn['Matched', ] # stop if error
  fom <- formula(x)
  # I cannot grab `addlvariables` yet..
  z1 <- sprintf(
    fmt = 'Nonparametric matching of `%s` based on %s was performed using <u>**`R`**</u> package <u>**`MatchIt`**</u> from %d `control` and %d `treated` subjects. The matched data contains %d `control` and %d `treated` subjects.',
    as.character(fom[[2L]]), # `arm`
    paste0('`', all.vars(fom[[3L]]), '`', collapse = ', '), # matching criterion
    n_all[1L], n_all[2L], n_matched[1L], n_matched[2L]
  ) |>
    new(Class = 'md_lines', package = 'MatchIt')
  
  z2 <- c(
    '```{r}', 
    '#| echo: false', 
    xnm |> sprintf(fmt = 'as_flextable(%s)'),
    '```'
  ) |>
    new(Class = 'md_lines')
  
  c(z1, z2) # ?rmd.tzh::c.md_lines
  
}





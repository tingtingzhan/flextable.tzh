

#' @title Convert \link[MatchIt]{matchit} to \link[flextable]{flextable}
#' 
#' @param x a \link[MatchIt]{matchit} object
#' 
#' @param fmt \link[base]{character} scalar, see function \link[base]{sprintf}
#' 
#' @param ... additional parameters of function `MatchIt:::summary.matchit`
#' 
#' @returns 
#' Functions [as_flextable.matchit] and [as_flextable.summary.matchit] both return a \link[flextable]{flextable}.
#' 
#' @examples
#' library(MatchIt)
#' # ?MatchIt::lalonde 
#' # `treat` is the treatment arm (0/1), `re78` is the outcome
#' matchit(treat ~ age+educ+race+nodegree+married+re74+re75, data = lalonde) |>
#'  as_flextable(addlvariables = 're78')
#' 
#' lalonde1 = within(lalonde, expr = {
#'   treat1 = as.logical(treat); treat = NULL
#' })
#' matchit(treat1 ~ age+educ+race+nodegree+married+re74+re75, data = lalonde1) |>
#'  as_flextable(addlvariables = 're78')
#'   
#' lalonde2 = within(lalonde, expr = {
#'   treat2 = structure(treat + 1L, levels = c('control', 'treated'), class = 'factor')
#'   treat = NULL
#' })
#' matchit(treat2 ~ age+educ+race+nodegree+married+re74+re75, data = lalonde2) |>
#'  as_flextable(addlvariables = 're78')
#' matchit(treat2 ~ age+educ+race+nodegree+married+re74+re75, data = lalonde2, ratio = 2) |>
#'  as_flextable(addlvariables = 're78')
#' 
#' @name flextable_matchit
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
#' @export as_flextable.summary.matchit
#' @export
as_flextable.summary.matchit <- function(x, fmt = '%.3f', ...) {
  
  mv <- all.vars(x$call$formula[[3L]]) # covariates to be matched; (m)atched-co(v)ariates
  
  out <- x$sum.matched # 'matrix'
  
  ret <- out
  storage.mode(ret) <- 'character'
  ret[] <- sprintf(fmt = fmt, out)
  ret[is.na(out)] <- NA_character_

  as_flextable.array(ret)
  
}

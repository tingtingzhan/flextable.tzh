

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
#' m1 = matchit(treat ~ age+educ+race+nodegree+married+re74+re75, data = lalonde)
#' m2 = matchit(treat ~ age+educ+race+nodegree+married+re74+re75, data = lalonde, ratio = 2)
#' m1 |> as_flextable()
#' m1 |> as_flextable(addlvariables = 're78')
#' m1 |> Sprintf.matchit()
#' m1 |> summary(addlvariables = 're78') |> Sprintf.summary.matchit()
#' m2 |> as_flextable()
#' m2 |> as_flextable(addlvariables = 're78')
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
  ret |> as_flextable.array()
}



#' @title Description of \link[MatchIt]{matchit} 
#' 
#' @param x \link[MatchIt]{matchit} object
#' 
#' @keywords internal
#' @name Sprintf_matchit
#' @export
Sprintf.matchit <- function(x) x |> summary() |> Sprintf.summary.matchit()

#' @rdname Sprintf_matchit
#' @importFrom stats formula
#' @export
Sprintf.summary.matchit <- function(x) {
  n_all <- x$nn['All', ] # stop if error
  n_matched <- x$nn['Matched', ] # stop if error
  fom <- formula(x)
  # I cannot grab `addlvariables` yet..
  sprintf(fmt = 'Nonparametric matching of `%s` based on %s was performed using <u>**`R`**</u> package <u>**`MatchIt`**</u> from %d `control` and %d `treated` subjects. The matched data contains %d `control` and %d `treated` subjects.',
          as.character(fom[[2L]]), # `arm`
          paste0('`', all.vars(fom[[3L]]), '`', collapse = ', '), # matching criterion
          n_all[1L], n_all[2L], n_matched[1L], n_matched[2L])
}



#' @title Convert \link[stats]{aov}, \link[stats]{anova} Objects To \link[flextable]{flextable}
#' 
#' @description
#' Convert \link[stats]{aov}, \link[stats]{anova}, or their \link[base]{summary} returns to \link[flextable]{flextable}.
#' 
#' 
#' @param x \link[stats]{aov}, \link[stats]{anova}, or their \link[base]{summary} returns
#' 
#' @param fmt \link[base]{character} scalar, see function \link[base]{sprintf}
#' 
#' @param row.title \link[base]{character} scalar
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @examples
#' # ?stats::aov
#' op = options(contrasts = c("contr.helmert", "contr.poly"))
#' class(m1 <- aov(yield ~ block + N*P*K, data = npk)) # 'aov'
#' as_flextable(m1)
#' aov(yield ~ block + N * P + K, npk) |> as_flextable()
#' class(m2 <- aov(yield ~  N*P*K + Error(block), data = npk)) # 'aovlist'
#' as_flextable(m2)
#' @name flextable_aov
#' @export as_flextable.anova
#' @export
as_flextable.anova <- function(x, fmt = '%.3f', row.title = ' ', ...) {
  # ?stats:::print.anova, then ?stats::printCoefmat
  x0 <- as.data.frame(x)
  x0[2:4] <- lapply(x0[2:4], FUN = sprintf, fmt = fmt)
  x0[['F value']][x0[['F value']] == 'NA'] <- ''
  x0[['Pr(>F)']] <- format_pval(x[['Pr(>F)']])
  .rowNamesDF(x0) <- x |> row.names.data.frame() |> trimws()
  x1 <- data.frame(' ' = row.names.data.frame(x0), x0, row.names = NULL, check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors = FALSE)
  names(x1)[1L] <- row.title
  as_flextable_dataframe(x1)
}

#' @rdname flextable_aov
#' @importFrom stats summary.aov
#' @export as_flextable.aov
#' @export
as_flextable.aov <- function(x, ...) {
  x |> summary.aov() |> as_flextable.summary.aov(...)
}


#' @rdname flextable_aov
#' @export as_flextable.summary.aov
#' @export
as_flextable.summary.aov <- function(x, ...) {
  if (length(x) != 1L) stop('deal with this')
  if (!inherits(x[[1L]], what = 'anova')) stop('deal with this')
  (x[[1L]]) |> as_flextable.anova(...)
}


#' @rdname flextable_aov
#' @export as_flextable.aovlist
#' @export
as_flextable.aovlist <- function(x, ...) {
  x |> summary() |> as_flextable.summary.aovlist(...)
  # ?stats:::summary.aovlist
}



#' @rdname flextable_aov
#' @export as_flextable.summary.aovlist
#' @export
as_flextable.summary.aovlist <- function(x, ...) {
  id <- vapply(x, FUN = inherits, what = 'summary.aov', FUN.VALUE = NA)
  if (!all(id)) stop('deal with this')
  .mapply(FUN = as_flextable.summary.aov, dots = list(
    x = x,
    row.title = names(x)
  ), MoreArgs = list(...))
}





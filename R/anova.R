
#' @title Convert \link[stats]{aov}, \link[stats]{anova} Objects To \link[flextable]{flextable}
#' 
#' @description
#' Convert \link[stats]{aov}, \link[stats]{anova}, or their \link[base]{summary} returns to \link[flextable]{flextable}.
#' 
#' @param x \link[stats]{aov}, \link[stats]{anova}, or their \link[base]{summary} returns
#' 
#' @param row.title \link[base]{character} scalar
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @name flextable_aov
#' @keywords internal
#' @importFrom flextable as_flextable flextable autofit colformat_double
#' @importFrom rmd.tzh label_pvalue_sym
#' @export as_flextable.anova
#' @export
as_flextable.anova <- function(x, row.title = ' ', ...) {
  
  # ?stats:::print.anova, then ?stats::printCoefmat
  
  x0 <- as.data.frame(x)
  x0[['Pr(>F)']] <- x[['Pr(>F)']] |> 
    label_pvalue_sym()()
  
  x1 <- data.frame(
    ' ' = x |> row.names.data.frame() |> trimws(), 
    x0, 
    row.names = NULL, check.names = FALSE, 
    fix.empty.names = FALSE, stringsAsFactors = FALSE)
  
  names(x1)[1L] <- row.title
  
  x1 |>
    flextable() |>
    autofit(part = 'all') |>
    colformat_double(j = 3:5, digits = 3L)
  
}

#' @rdname flextable_aov
#' @importFrom flextable as_flextable
#' @importFrom stats summary.aov
#' @export as_flextable.aov
#' @export
as_flextable.aov <- function(x, ...) {
  x |> 
    summary.aov() |> 
    as_flextable.summary.aov(...)
}


#' @rdname flextable_aov
#' @importFrom flextable as_flextable
#' @export as_flextable.summary.aov
#' @export
as_flextable.summary.aov <- function(x, ...) {
  if (length(x) != 1L) stop('deal with this')
  if (!inherits(x[[1L]], what = 'anova')) stop('deal with this')
  (x[[1L]]) |> 
    as_flextable.anova(...)
}


#' @rdname flextable_aov
#' @importFrom flextable as_flextable
#' @export as_flextable.aovlist
#' @export
as_flextable.aovlist <- function(x, ...) {
  x |> summary() |> as_flextable.summary.aovlist(...)
  # ?stats:::summary.aovlist
}



#' @rdname flextable_aov
#' @importFrom flextable as_flextable
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





#' @title R Markdown Lines for \link[stats]{aov}
#' 
#' @param x,xnm,... ..
#' 
#' @export md_.aov
#' @export
md_.aov <- function(x, xnm, ...) {
  
  z1 <- 'Analysis of variance is performed using <u>**`R`**</u>.' |>
    new(Class = 'md_lines')
  
  z2 <- c(
    '```{r}', 
    '#| echo: false', 
    xnm |> sprintf(fmt = 'as_flextable(%s)'), # flextable.tzh::as_flextable.aov
    '```'
  ) |>
    new(Class = 'md_lines')
  
  c(z1, z2) # ?rmd.tzh::c.md_lines
  
}






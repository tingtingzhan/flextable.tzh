

#' @title Convert \link[stats]{TukeyHSD} into \link[flextable]{flextable}
#' 
#' @description
#' Convert \link[stats]{TukeyHSD} into a \link[flextable]{flextable}.
#' 
#' @param x a \link[stats]{TukeyHSD}
#' 
#' @param row.title \link[base]{character} scalar, 
#' default is the left-hand-side of \link[stats]{aov} formula
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @keywords internal
#' @importFrom flextable as_flextable
#' @importFrom scales.tzh label_pvalue_sym
#' @export as_flextable.TukeyHSD
#' @export
as_flextable.TukeyHSD <- function(
    x, 
    row.title = deparse1(attr(x, which = 'orig.call', exact = TRUE)$formula[[2L]]),
    ...
) {
  
  x0 <- unclass(x)
  
  conf.level <- attr(x, which = 'conf.level', exact = TRUE)
  
  ret0 <- .mapply(FUN = \(x, nm) {
    tmp <- x
    tmp[,1L] <- sprintf(fmt = '%.2f (%.2f, %.2f)', x[,1L], x[,2L], x[,3L])
    tmp[,4L] <- x[,4L] |> label_pvalue_sym()()
    ret <- tmp[, c(1L, 4L), drop = FALSE]
    dimnames(ret) <- list(
      sprintf(fmt = '%s \u2e22%s\u2e25', nm, rownames(tmp)),
      c(sprintf(fmt = 'Difference (%.0f%% CI)', 1e2*conf.level), 'Signif.')
    )
    return(ret)
  }, dots = list(x = x0, nm = names(x0)), MoreArgs = NULL)
  # list of 'matrix'
  
  #if (length(x0) == 1L) or not..
  ret0 |>
    do.call(what = rbind) |>
    as_flextable.array(row.title = row.title) |>
    hline(i = vapply(x0, FUN = nrow, FUN.VALUE = NA_integer_)[-length(x0)])
  
}







#' @title Alternative Conversion from \link[base]{data.frame} to \link[flextable]{flextable}
#' 
#' @description
#' ..
#' 
#' @param x \link[base]{data.frame}
#' 
# @param hline_i ..
#' 
# @param vline_j ..
#' 
#' @param ... ..
#' 
#' @note
#' Name clash `flextable:::as_flextable.data.frame`.
#' 
#' @returns
#' Function [as_flextable_dataframe] returns a \link[flextable]{flextable}.
#' 
#' @keywords internal
#' @importFrom flextable flextable autofit hline vline
#' @export
as_flextable_dataframe <- function(
    x,
    #hline_i = integer(0L), 
    #vline_j = attr(x, which = 'vline_j', exact = TRUE) %||% integer(0L),
    ...
) {
  
  nr <- .row_names_info(x, type = 2L)
  
  #if (is.call(hline_i) && hline_i[[1L]] == '~') {
  #  # e.g., `hline_i` being `~ x1 + x2`
  #  # ?base::sort_by.data.frame sort by `x1` first, then `x2`
  #  # ?base::.formula2varlist returns a list of name c('x1', 'x2')
  #  # ?base::split.default splits by `x1` varies fast, and `x2` varies slow; which is not want I want!!! 
  #  # therefore, I use ?base::rev.default, which is super smart!!
  #  x <- sort_by.data.frame(x, y = hline_i) # various attributes retained
  #  tmp <- split.default(seq_len(nr), f = rev.default(.formula2varlist(formula = hline_i, data = x)))
  #  hline_i <- cumsum(lengths(tmp, use.names = FALSE))
  #}
  
  #if (!length(hline_i)) hline_i <- integer(0L)
  
  #if (!is.integer(hline_i)) stop('`hline_i` must be convertible to integer')
  #hline_i <- setdiff(hline_i, nr)
  
  x |>
    format2flextable() |>
    flextable() |>
    autofit(part = 'all') #|>
    #hline(i = hline_i) |>
    #vline(j = vline_j)
  
}












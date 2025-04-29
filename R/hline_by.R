

#' @title hline_by
#' 
#' @param x a \link[flextable]{flextable}
#' 
#' @param i \link[stats]{formula}
#' 
#' @param ... additional parameters, currently no use
#' 
#' @keywords internal
#' @export
hline_by <- function(x, i, ...) {
  
  d <- x$body$dataset
  if (!is.data.frame(d)) stop('wont happen')
  
  nr <- .row_names_info(d, type = 2L)
  
  if (!is.call(i) || i[[1L]] != '~') stop('argument `i` must be formula')
  # e.g., `i` being `~ x1 + x2`
  # ?base::sort_by.data.frame sort by `x1` first, then `x2`
  # ?base::.formula2varlist returns a list of name c('x1', 'x2')
  # ?base::split.default splits by `x1` varies fast, and `x2` varies slow; which is not want I want!!! 
  # therefore, I use ?base::rev.default, which is super smart!!
  d_ <- d |> 
    sort_by.data.frame(y = i) # various attributes retained
  if (!identical(d, d_)) stop('sort original data.frame first..')
  # tzh does not know how to [sort_by.flextable] 
  
  f <- d |> 
    .formula2varlist(formula = i) |> 
    rev.default()
  
  h <- nr |>
    seq_len() |>
    split.default(f = f) |>
    lengths(use.names = FALSE) |>
    cumsum()
  
  if (!length(h)) h <- integer(0L)
  
  if (!is.integer(h)) stop('`h` must be convertible to integer')
  h <- setdiff(h, nr)
  
  x |>
    hline(i = h)
  
}




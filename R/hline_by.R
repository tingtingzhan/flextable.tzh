

#' @title hline_by
#' 
#' @param x a \link[base]{data.frame} or \link[flextable]{flextable}
#' 
#' @param i \link[stats]{formula}
#' 
#' @param ... additional parameters, currently no use
#' 
#' @examples
#' datasets::sleep |>
#'  hline_by(i = ~ group)
#' 
#' datasets::women |>
#'  hline_by(i = ~ cut(height, breaks = c(55, 60, 65, 70)))
#' 
#' datasets::Puromycin |>
#'  hline_by(i = ~ state + conc)
#' @keywords internal
#' @name hline_by
#' @export
hline_by <- function(x, ...) UseMethod(generic = 'hline_by')

#' @rdname hline_by
#' @importFrom flextable flextable autofit
#' @export hline_by.data.frame
#' @export
hline_by.data.frame <- function(x, ...) {
  x |>
    flextable() |>
    autofit(part = 'all') |>
    hline_by.flextable(...)
}


#' @rdname hline_by
#' @export hline_by.flextable
#' @export
hline_by.flextable <- function(x, i, ...) {
  
  d <- x$body$dataset
  if (!is.data.frame(d)) stop('wont happen')
  
  nr <- .row_names_info(d, type = 2L)
  
  if (!is.call(i) || i[[1L]] != '~') stop('`i` must be formula')
  
  # see inside ?base::sort_by.data.frame
  f <- i |>
    .formula2varlist(formula = _, data = d) |> # always 'list'
    unname()
  
  o <- do.call(what = order, args = f)
  if (!identical(o, seq_along(o))) stop('sort original data.frame first..')
  
  f |> 
    interaction(drop = TRUE, lex.order = FALSE) |> # see inside ?base::split.default
    table() |> # already sorted
    cumsum() |>
    setdiff(y = nr) |>
    hline(x = x, i = _)
  
}




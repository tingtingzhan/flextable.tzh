


#' @title highlight_by
#' 
#' @param x a \link[base]{data.frame} or \link[flextable]{flextable}
#' 
#' @param i \link[base]{logical} \link[base]{expression} indicating rows to \link[flextable]{highlight}: missing values are taken as false.
#' 
#' @param ... additional parameters for function \link[flextable]{highlight}
#' 
#' @examples
#' datasets::attitude |>
#'  highlight_by(i = learning > 55 | raises > 65, j = 'rating')
#'  
#' datasets::airquality |>
#'  head(n = 10L) |> # with missing
#'  highlight_by(i = Ozone > 20, j = c('Month', 'Day'))
#' @keywords internal
#' @name highlight_by
#' @export
highlight_by <- function(x, ...) UseMethod(generic = 'highlight_by')


#' @rdname highlight_by
#' @importFrom flextable flextable autofit
#' @export highlight_by.data.frame
#' @export
highlight_by.data.frame <- function(x, ...) {
  x |>
    flextable() |>
    autofit(part = 'all') |>
    highlight_by.flextable(...)
}


#' @rdname highlight_by
#' @importFrom flextable highlight
#' @export highlight_by.flextable
#' @export
highlight_by.flextable <- function(x, i, ...) {
  
  d <- x$body$dataset
  if (!is.data.frame(d)) stop('wont happen')
  
  e <- substitute(i)
  
  x |> 
    highlight(i = eval(e, envir = d), ...)

}









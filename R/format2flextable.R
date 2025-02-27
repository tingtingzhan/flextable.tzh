
#' @title format2flextable
#' 
#' @param x \link[base]{data.frame}
#' 
#' @returns 
#' Function [format2flextable] returns a \link[base]{data.frame}
#' 
#' @examples
#' library(survival)
#' aml2 = within(aml, expr = {
#'   edp = Surv(time, status)
#'   time = status = NULL
#' })
#' 
#' # ?flextable:::as_flextable.data.frame cannot process 'matrix' columns, as of 2024-10-27
#' tryCatch(as_flextable(aml2), error = identity)
#' 
#' aml2 |> format2flextable() |> as_flextable()
#' @keywords internal
#' @export
format2flextable <- function(x) {
  x[] <- lapply(x, FUN = function(ix) {
    if (inherits(ix, what = 'Surv')) {
      format(ix) 
      # ?survival::format.Surv # convert 'Surv' (inherits from 'matrix') to 'character'
    } else ix
  })
  return(x)
}

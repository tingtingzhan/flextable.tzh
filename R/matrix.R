
#' @title Convert a \link[base]{matrix} to \link[flextable]{flextable} 
#' 
#' @description ..
#' 
#' @param x a \link[base]{matrix}
#' 
#' @param row.title \link[base]{character} scalar
#' 
#' @param ... ..
#' 
#' @returns 
#' Function [as_flextable.matrix()] returns a \link[flextable]{flextable}.
#' 
#' @keywords internal
#' @importFrom flextable flextable as_flextable init_flextable_defaults autofit hline vline fix_border_issues align add_header_row merge_h merge_v
#' @importFrom officer fp_border
#' @export as_flextable.matrix
#' @export
as_flextable.matrix <- function(
    x, 
    row.title = if (has_DNM[1L]) DNM[1L] else ' ',
    ...
) {

  x_orig <- x # just in case
  
  dmx <- dim(x)
  
  #nd <- length(dmx)
  nd <- 2L # must be 'matrix'
  
  x0 <- unclass(x) # e.g. 'table', 'noquote'
  # ?base::as.data.frame.table is not what I want
  
  # init_flextable_defaults()$na_str
  # set_flextable_defaults(na_str = '<NA>')

  dnm <- dimnames(x0)
  rnm <- rownames(x0)

  if (!length(DNM <- names(dnm))) DNM <- character(length = nd)
  # empty names(dimnames) will be zchar (R 4.1.1)
  has_DNM <- nzchar(DNM)
  
  x1 <- x0 |>
    as.data.frame.matrix(make.names = FALSE, row.names = FALSE, stringsAsFactors = FALSE)
  
  if (length(rnm)) {
    x2 <- data.frame(' ' = rownames(x0), x1, row.names = NULL, check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors = FALSE)
    #if (has_DNM[1L]) names(x2)[1L] <- DNM[1L]
    names(x2)[1L] <- row.title
    # ?flextable::flextable -> ?flextable:::complex_tabpart
    # line 19: `data[col_keys]` will not allow zchar colnames
  } else x2 <- x1
  
  border_hard_ <- fp_border(
    width = 2 * init_flextable_defaults()$border.width, # *looks* like default border width used in ?flextable::flextable
    color = init_flextable_defaults()$border.color # '#666666', i.e. 'gray40'
  )

  y0 <- x2 |> 
    flextable() |>
    autofit(part = 'all') |>
    vline(j = if (length(rnm)) 1L else integer(), border = border_hard_) # row.names becomes col-1
  
  if (!has_DNM[2L]) return(y0)
  
  y0 |> 
    add_header_row(top = TRUE, values = c(row.title, rep(DNM[2L], times = dmx[2L]))) |>
    merge_h(part = 'header') |>
    merge_v(part = 'header') |>
    align(align = 'center', part = 'header') |>
    fix_border_issues() # inspired by ?flextable::theme_vanilla
  
}




#' @title as_flextable.noquote
#' 
#' @param x a \link[base]{noquote} object
#' 
#' @param ... additional parameters of S3 generic \link[flextable]{as_flextable}
#' 
#' @returns 
#' Function [as_flextable.noquote()] returns a \link[flextable]{flextable}.
#' 
#' @keywords internal
#' @importFrom flextable as_flextable
#' @export as_flextable.noquote
#' @export
as_flextable.noquote <- function(x, ...) {
  x |> unclass() |> as_flextable(...)
}





#if (inherits(x, 'matrix') && (typeof(x) == 'integer')) {
#  .Defunct(msg = '|> as.table() |> flextable:::as_flextable.table(...)')
#} # present this in a different way


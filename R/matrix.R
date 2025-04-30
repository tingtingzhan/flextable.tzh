
#' @title Convert a \link[base]{double} or \link[base]{character} \link[base]{matrix} to \link[flextable]{flextable} 
#' 
#' @description ..
#' 
#' @param x a \link[base]{double} or \link[base]{character} \link[base]{array}
#' 
#' @param row.title \link[base]{character} scalar
#' 
# @param hline_i,vline_j ..
#' 
#' @param ... ..
#' 
#' @returns 
#' Function [as_flextable.matrix()] returns a \link[flextable]{flextable}.
#' 
#' @keywords internal
#' @importFrom flextable flextable as_flextable autofit hline vline fix_border_issues align add_header_row merge_h merge_v
#' @importFrom officer fp_border
#' @export as_flextable.matrix
#' @export
as_flextable.matrix <- function(
    x, 
    row.title = if (has_DNM[1L]) DNM[1L] else ' ',
    #hline_i = integer(0L), vline_j = integer(0L),
    ...
) {

  if (typeof(x) == 'integer') {
    .Defunct(msg = '|> as.table() |> flextable:::as_flextable.table(...)')
  }
  
  x_orig <- x # just in case
  
  dmx <- dim(x)
  
  #nd <- length(dmx)
  nd <- 2L # must be 'matrix'
  
  x0 <- unclass(x) # e.g. 'table', 'noquote'
  # ?base::as.data.frame.table is not what I want
  if (length(dnm <- dimnames(x0))) {
    for (i in seq_len(nd)) {
      if (length(dnm[[i]]) && any(rna <- is.na(dnm[[i]]))) dnm[[i]][rna] <- '<NA>'
    }
    dimnames(x0) <- dnm
  }
  
  if (!length(DNM <- names(dnm))) DNM <- character(length = nd)
  # empty names(dimnames) will be zchar (R 4.1.1)
  
  dnm <- dimnames(x0)
  
  x1 <- as.data.frame.matrix(x0, make.names = FALSE, stringsAsFactors = FALSE)
  
  has_DNM <- nzchar(DNM)
  
  x2 <- data.frame(' ' = row.names.data.frame(x1), x1, row.names = NULL, check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors = FALSE)
  #if (has_DNM[1L]) names(x2)[1L] <- DNM[1L]
  names(x2)[1L] <- row.title
  # ?flextable::flextable -> ?flextable:::complex_tabpart
  # line 19: `data[col_keys]` will not allow zchar colnames
  
  #if (length(vline_j)) {
  #  # row.names becomes col-1
  #  # but never put a vline at the right-end of table
  #  vline_j <- setdiff(x = vline_j+1L, y = length(x2)) 
  #} # else do nothing
  
  #if (length(hline_i)) {
  #  # never put an hline at the bottom of table
  #  hline_i <- setdiff(x = hline_i, y = nrow(x0))
  #}
  
  border_hard_ <- fp_border(width = 1.5, color = 'gray40')
  # *looks* like default border used in ?flextable::flextable
  # tzh does *not* know how to find out for sure, for now
  border_soft_ <- fp_border(width = .5, color = 'gray40')
  
  y0 <- x2 |> 
    flextable() |>
    autofit(part = 'all') |>
    #hline(i = hline_i) |>
    #vline(j = vline_j) |>
    vline(j = 1L, border = border_hard_)
  
  if (!has_DNM[2L]) return(y0)
  
  y0 |> 
    add_header_row(top = TRUE, values = c(row.title, rep(DNM[2L], times = dmx[2L]))) |>
    merge_h(part = 'header') |>
    merge_v(part = 'header') |>
    align(align = 'center', part = 'header') |>
    fix_border_issues() #|> # inspired by ?flextable::theme_vanilla
  #vline(j = 1L) |> # this cannot be ?flextable::fix_border_issues -ed
  #hline_top(part = 'header') |> # no longer needed
  #bold(bold = TRUE, part = 'header') # no longer needed
  
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


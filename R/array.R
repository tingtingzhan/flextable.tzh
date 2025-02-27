


#' @title as_flextable.array
#' 
#' @description ..
#' 
#' @param x a \link[base]{array}
#' 
#' @param row.title \link[base]{character} scalar
#' 
#' @param hline_i,vline_j ..
#' 
#' @param ... ..
#' 
#' @returns 
#' Function [as_flextable.array] returns a \link[flextable]{flextable}.
#' 
#' @examples
#' as_flextable(VADeaths) # 'matrix'
#' as_flextable(occupationalStatus) # ?flextable:::as_flextable.table
#' as_flextable.array(occupationalStatus)
#' 
#' tryCatch(as_flextable(UCBAdmissions), error = identity) # ?flextable:::as_flextable.table
#' tryCatch(as_flextable.array(UCBAdmissions), error = identity)
#' @importFrom officer fp_border
#' @export as_flextable.array
#' @export
as_flextable.array <- function(
    x, 
    row.title = if (has_DNM[1L]) DNM[1L] else ' ',
    hline_i = integer(0L), vline_j = integer(0L),
    ...
) {
  
  x_orig <- x # just in case
  if (length(dmx <- dim(x)) == 1L) {
    x <- array(x_orig, dim = c(1L, dmx), dimnames = c(list(NULL), dimnames(x_orig)))
    dmx <- dim(x)
  }
  nd <- length(dmx)
  
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
  
  if (nd > 2L) {
    # this does not work well with ?rmarkdown::render
    #return(lapply(seq_len(dmx[[3L]]), FUN = function(i) {
    #  ix <- array(x0[, , i, drop = TRUE], dim = dmx[1:2], dimnames = dnm[1:2])
    #  as_flextable.array(ix) |> flextable::set_caption(caption = sprintf(fmt = '%s = %s', DNM[3L], dnm[[3L]][i]))
    #}))
    # end of not working well with ?rmarkdown::render
    stop('array must have maximum two dimensions') # following ?flextable:::as_flextable.table
  }
  
  if (!is.matrix(x0)) stop('input cannot be turned into matrix') # before 2023-08-09, should be typo
  
  dnm <- dimnames(x0)
  
  # Cohen's kappa
  # was in [as_flextable.xtabs], but no longer
  
  x1 <- as.data.frame.matrix(x0, make.names = FALSE, stringsAsFactors = FALSE)
  
  has_DNM <- nzchar(DNM)
  
  x2 <- data.frame(' ' = row.names.data.frame(x1), x1, row.names = NULL, check.names = FALSE, fix.empty.names = FALSE, stringsAsFactors = FALSE)
  #if (has_DNM[1L]) names(x2)[1L] <- DNM[1L]
  names(x2)[1L] <- row.title
  
  vline_j <- if (length(vline_j)) {
    # row.names becomes col-1
    # but never put a vline at the right-end of table
    setdiff(x = vline_j+1L, y = length(x2)) 
  } else integer()
  
  border_hard_ <- fp_border(width = 1.5, color = 'gray40')
  # *looks* like default border used in ?flextable::flextable
  # tzh does *not* know how to find out for sure, for now
  border_soft_ <- fp_border(width = .5, color = 'gray40')
  
  y0 <- x2 |> 
    flextable() |>
    autofit(part = 'all') |>
    hline(i = hline_i) |>
    vline(j = vline_j) |>
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
#' @param ... additional parameters of function [as_flextable.array]
#' 
#' @returns 
#' Function [as_flextable.noquote] returns a \link[flextable]{flextable}.
#' 
#' @export as_flextable.noquote
#' @export
as_flextable.noquote <- function(x, ...) {
  if (!is.array(x)) stop('input must be \'array\'')
  as_flextable.array(unclass(x), ...)
}


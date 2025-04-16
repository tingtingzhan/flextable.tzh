
#' @title Convert \link[stats]{ftable} to \link[flextable]{flextable}
#' 
#' @param x an \link[stats]{ftable}
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns
#' Function [as_flextable.ftable] returns a \link[flextable]{flextable}.
#' 
#' @examples
#' esoph0 = esoph[c('tobgp', 'ncases', 'ncontrols')]
#' (x0 = ftable(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph0)))
#' x0 |> as_flextable()
#' (x = ftable(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph)))
#' x |> as_flextable()
#' @keywords internal
#' @importFrom flextable flextable as_flextable autofit hline vline merge_v
#' @importFrom zoo na.locf
#' @export as_flextable.ftable
#' @export
as_flextable.ftable <- function(x, ...) {
  
  atr <- attributes(x)
  nr <- length(atr$row.vars) # lowest group
  rseq <- seq_len(nr)
  
  xf <- format(x, quote = FALSE, method = 'compact', lsep = '', ...) # ?stats:::format.ftable
  xf[] <- trimws(xf)
  cnm <- xf[1L, , drop = TRUE]
  xf <- xf[-1L, , drop = FALSE]
  colnames(xf) <- cnm
  xf[,rseq][!nzchar(xf[,rseq])] <- NA_character_

  h_i <- seq.int(from = 0, to = nrow(xf), by = length(atr$row.vars[[nr]]))
  
  xf |>
    as.data.frame.matrix() |>
    na.locf(na.rm = FALSE) |> # invokes ?zoo:::na.locf.data.frame
    flextable() |>
    autofit(part = 'all') |>
    vline(j = nr) |>
    hline(i = h_i[-c(1L, length(h_i))]) |>
    merge_v(part = 'body', j = seq_len(nr))
  
}






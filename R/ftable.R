
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
#' as_flextable(x0)
#' (x = ftable(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph)))
#' as_flextable(x)
#' @importFrom flextable flextable vline hline merge_v
#' @importFrom zoo na.locf
#' @export as_flextable.ftable
#' @export
as_flextable.ftable <- function(x, ...) {
  
  atr <- attributes(x)
  nr <- length(atr$row.vars) # lowest group
  
  xf <- format(x, quote = FALSE, method = 'compact', lsep = '', ...) # ?stats:::format.ftable
  xf[] <- trimws(xf)
  cnm <- xf[1L, , drop = TRUE]
  xf <- xf[-1L, , drop = FALSE]
  colnames(xf) <- cnm

  h_i <- seq.int(from = 0, to = nrow(xf), by = length(atr$row.vars[[nr]]))
  
  xf2 <- as.data.frame.matrix(xf)
  xf2[] <- lapply(X = xf2, FUN = function(i) {
    i[!nzchar(i)] <- NA_character_
    na.locf(i)
  })

  xf2 |>
    flextable() |>
    autofit(part = 'all') |>
    vline(j = nr) |>
    hline(i = h_i[-c(1L, length(h_i))]) |>
    merge_v(part = 'body', j = seq_len(nr))
  
}








#' @title Format \eqn{p}-values with Significance Symbol
#' 
#' @description
#' Format \eqn{p}-values with significance symbol.
#' 
#' @param x R object of \link[base]{typeof} \link[base]{numeric}
#' 
#' @param add_p,... see function \link[scales]{label_pvalue}
#' 
#' @param add_symbol \link[base]{logical} scalar, whether to add Unicode
#' significance symbol.  Default `TRUE`
#' 
#' @details
#' Workhorse of \eqn{p}-value formatting is function \link[scales]{label_pvalue}.
#' 
#' Workhorse of significance symbol is function \link[stats]{symnum} (also see function \link[stats]{printCoefmat}).
#' 
#' @returns 
#' Function [format_pval()] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples 
#' p1 = p2 = c(pi^-100, .02, .05, .1, .9999, NA_real_)
#' dim(p1) = c(2, 3); p1
#' names(p2) = letters[1:6]; p2
#' format_pval(p1) # attr-dim kept
#' format_pval(p2) # attr-name kept
#' format_pval(p1, add_p = TRUE)
#' format_pval(p2, add_p = TRUE, add_symbol = FALSE)
#' # below: not preferable
#' base::format.pval(p1, na.form = '', digits = 3L)
#' base::format.pval(p2, na.form = '', digits = 4L)
#' # below: exception handling
#' format_pval(double())
#' @importFrom scales label_pvalue
#' @importFrom stats symnum
#' @export
format_pval <- function(x, add_p = FALSE, add_symbol = TRUE, ...) {

  ret <- x
  storage.mode(ret) <- 'character'
  
  if (!length(x)) return(ret)
  
  ret[] <- x |> 
    label_pvalue(add_p = add_p, ...)() |>
    sub(pattern = '([-]?)0[.]', replacement = '\\1.') # http://stackoverflow.com/questions/12643391
  
  if (add_symbol) {
    sym <- symnum(
      x, corr = FALSE, na = FALSE, 
      cutpoints = c(0, .001, .01, .05, .1, 1), 
      #symbols = c('\u2605\u2605\u2605', '\u2605\u2605', '\u2605', '\u2606', '') # star
      symbols = c('\u2b51\u2b51\u2b51', '\u2b51\u2b51', '\u2b51', '\u2b52', '') # small star
    ) # see ?stats::printCoefmat
    ret[] <- ret |> paste(sym) |> trimws()
  } # else do nothing
  
  ret[is.na(x)] <- '' # *not* NA_character_
  
  return(ret)
  
}








#' @title Format \eqn{p}-values with Significance Symbol
#' 
#' @description
#' Format \eqn{p}-values with significance symbol.
#' 
#' @param x R object of \link[base]{typeof} \link[base]{numeric}
#' 
#' @param add_p,... see function \link[scales]{label_pvalue}
#' 
#' @details
#' Workhorse of \eqn{p}-value formatting is function \link[scales]{label_pvalue}.
#' 
#' Workhorse of significance symbol is function \link[stats]{symnum} (also see function \link[stats]{printCoefmat}).
#' 
#' 
#' @returns 
#' Function [format_pval] returns a \link[base]{character} \link[base]{vector}.
#' 
#' @examples 
#' (p = setNames(c(pi^-100, .02, .05, .1, .9999, NA_real_), letters[1:5]))
#' format_pval(p)
#' format_pval(p, add_p = TRUE)
#' # below: not preferable
#' base::format.pval(p, na.form = '', digits = 3L)
#' base::format.pval(p, na.form = '', digits = 4L)
#' @importFrom scales label_pvalue
#' @importFrom stats symnum
#' @export
format_pval <- function(x, add_p = FALSE, ...) {
  
  ret0 <- label_pvalue(add_p = add_p, ...)(x) |>
    sub(pattern = '([-]?)0[.]', replacement = '\\1.') # my [dropleading0]
  
  sym <- symnum(
    x, corr = FALSE, na = FALSE, 
    cutpoints = c(0, .001, .01, .05, .1, 1), 
    #symbols = c('\u2605\u2605\u2605', '\u2605\u2605', '\u2605', '\u2606', '') # star
    symbols = c('\u2b51\u2b51\u2b51', '\u2b51\u2b51', '\u2b51', '\u2b52', '') # small star
  ) # see ?stats::printCoefmat
  
  ret <- trimws(paste(ret0, sym))
  ret[is.na(x)] <- '' # *not* NA_character_
  return(ret)
  
}






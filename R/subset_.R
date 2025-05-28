
# this is a simplified version of experiment::SUBSET

#' @title Inspect a Subset of \link[base]{data.frame}
#' 
#' @description ..
#' 
#' @param x a \link[base]{data.frame}
#' 
#' @param subset \link[base]{logical} \link[base]{expression},
#' see function \link[base]{subset.data.frame}
#' 
#' @param select \link[base]{character} \link[base]{vector},
#' columns to be selected,
#' see function \link[base]{subset.data.frame}
#' 
#' @param select_pattern regular expression \link[base]{regex}
#' for multiple columns to be selected
#' 
#' @param avoid \link[base]{character} \link[base]{vector},
#' columns to be avoided
#' 
#' @param avoid_pattern regular expression \link[base]{regex},
#' for multiple columns to be avoided
#' 
#' @details 
#' Function [subset_()] is different from function 
#' \link[base]{subset.data.frame}, such that 
#' \itemize{
#' \item {if both `select` and `select_pattern` are missing, only variables mentioned in `subset` are selected;}
#' \item {be able to select all variables, except those in `avoid` and `avoid_pattern`.}
#' }
#' 
#' @returns
#' Function [subset_()] returns a \link[flextable]{flextable}.
#' 
#' @keywords internal
#' @importFrom flextable flextable autofit highlight vline
#' @importFrom cli col_cyan style_bold
#' @export
subset_ <- function(
    x, subset, 
    select, select_pattern, 
    avoid, avoid_pattern
) {
  
  if (!is.data.frame(x)) stop('input must be \'data.frame\'')
  if (isS4(x)) stop('Use S3 data.frame explicitly')
  
  nm <- names(x)
  .subset <- substitute(subset)

  select <- c(
    if (!missing(select)) select, 
    if (!missing(select_pattern)) grepv(select_pattern, x = nm)
  )
  
  avoid <- c(
    if (!missing(avoid)) avoid, 
    if (!missing(avoid_pattern)) grepv(avoid_pattern, x = nm)
  )
  
  var_subset <- intersect(all.vars(.subset), nm)
  
  var_sel <- if (!length(avoid)) {
    c(var_subset, select)
  } else if (!length(select)) {
    # `avoid`, but no `select`
    c(var_subset, setdiff(nm, avoid))
  } else {
    # `avoid`, `select`
    setdiff(x = c(var_subset, select), y = avoid)
  }
  var_sel <- nm[sort.int(match(unique.default(var_sel), table = nm))] # in original order
  
  rid <- which(eval(expr = .subset, envir = x)) # removes NA
  if (!length(rid)) {
    message('No subject satisfies that ', .subset |> deparse1() |> col_cyan() |> style_bold())
    return(invisible())
  }
  
  data.frame(
    '_Excel_' = sprintf(fmt = 'Row %d', rid + 1L),
    x[rid, var_sel, drop = FALSE],
    check.names = FALSE
  ) |>
    flextable() |>
    autofit(part = 'all') |>
    vline(j = 1L) |>
    highlight(j = setdiff(var_subset, avoid), part = 'all')
  
}

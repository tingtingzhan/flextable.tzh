
# this is a simplified version of tzh's experiment::::SUBSET

#' @title Alternative Subset of \link[base]{data.frame}
#' 
#' @description 
#' An alternative subset method for \link[base]{data.frame}.
#' 
#' @param x a \link[base]{data.frame}
#' 
#' @param subset \link[base]{logical} \link[base]{expression},
#' see function \link[base]{subset.data.frame}
#' 
#' @param select \link[base]{character} \link[base]{vector},
#' columns to be selected,
#' see function \link[base]{subset.data.frame}.
#' Default is `character()`, indicating no variable to be selected in addition to those appear in `subset`.
#' 
#' @param select_rx regular expression \link[base]{regex}
#' for multiple columns to be selected. 
#' Default is the [negative lookahead](https://en.wikipedia.org/wiki/Regular_expression#Assertions) `'?!'`
#' 
#' @param avoid \link[base]{character} \link[base]{vector},
#' columns to be avoided.
#' Default is `character()`, indicating no variable to be avoid.
#' The variables that appear in `subset` cannot be avoided.
#' 
#' @param avoid_rx regular expression \link[base]{regex},
#' for multiple columns to be avoided
#' Default is the [negative lookahead](https://en.wikipedia.org/wiki/Regular_expression#Assertions) `'?!'`
#' 
#' @param preview \link[base]{logical} scalar, whether to preview as a 
#' \link[flextable]{flextable}.  Default `FALSE`.
#' 
#' @param ... additional parameters, currently of no use
#' 
#' @details 
#' Function [subset_()] is different from function 
#' \link[base]{subset.data.frame}, such that 
#' \itemize{
#' \item {if both `select` and `select_rx` are missing, only variables mentioned in `subset` are selected;}
#' \item {be able to select all variables, except those in `avoid` and `avoid_rx`.}
#' }
#' 
#' @returns
#' Function [subset_()] returns a \link[flextable]{flextable}.
#' 
#' @keywords internal
#' @importFrom flextable flextable autofit highlight vline
#' @export
subset_ <- function(
    x, subset = NULL, 
    select = character(), select_rx = '?!', 
    avoid = character(), avoid_rx = '?!', 
    preview = FALSE,
    ...
) {
  
  if (!is.data.frame(x)) stop('input must be \'data.frame\'')
  if (isS4(x)) stop('Use S3 data.frame explicitly')
  
  nm <- names(x)
  .subset <- substitute(subset)
  
  v_subset <- intersect(all.vars(.subset), nm)
  
  v_to_select <- c(select, grepv(select_rx, x = nm)) |>
    unique.default()
  
  v_after_avoid <- c(avoid, grepv(avoid_rx, x = nm)) |>
    unique.default() |>
    setdiff(x = v_to_select, y = _) # beautiful!!
  
  id_sel <- c(v_subset, v_to_select, v_after_avoid) |> # beautiful!!
    unique.default() |>
    match(table = nm) |>
    sort.int() # in original order
  
  rid <- if (!length(.subset)) {
    # all rows
    x |> 
      .row_names_info(type = 2L) |>
      seq_len()
  } else {
    .subset |> 
      eval(envir = x) |>
      which() # removes NA
  }
  
  if (!length(rid)) {
    message('No subject satisfies that ', .subset |> deparse1() |> col_cyan() |> style_bold())
    return(invisible())
  }
  
  ret <- x[rid, nm[id_sel], drop = FALSE]
  if (!preview) return(ret)
  
  data.frame(
    '_Excel_' = sprintf(fmt = 'Row %d', rid + 1L),
    ret,
    check.names = FALSE
  ) |>
    flextable() |>
    autofit(part = 'all') |>
    vline(j = 1L) |>
    highlight(j = v_subset, part = 'all')
  
}

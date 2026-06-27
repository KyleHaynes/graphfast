#' @keywords internal
#' @useDynLib graphfast, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom methods new
#' @importFrom data.table data.table copy := is.data.table melt
#' @importFrom fastmatch fmatch
#' @importFrom utils head
#' @importFrom stats setNames
"_PACKAGE"

# Silence R CMD check NOTEs for data.table's non-standard evaluation (column
# names referenced bare inside data.table calls are not global variables).
utils::globalVariables(c(
  ".SD", "..cols", "id", "id2", "value", "variable", "group_id"
))

## usethis namespace: start
## usethis namespace: end
NULL
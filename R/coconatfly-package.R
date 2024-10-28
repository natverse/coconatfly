#' @keywords internal
#' @import nat
#' @section Package options:
#'
#' coconatfly itself currently only has one option, which is to use a historical
#' behaviour for collating cell types for multi-connectome cosine clustering.
#'
#' \itemize{
#'
#' \item \code{options(coconatfly.cluster.dropna)}
#'
#' }
#'
#' @examples
#' # list any package options that have been set
#' options()[grepl("^coconatfly.*", names(options()))]
#'
#' # return to behaviour of coconatfly < v0.2
#' options(coconatfly.cluster.dropna=FALSE)
#' # return to default behaviour of coconatfly >= v0.2
#' options(coconatfly.cluster.dropna=TRUE)
#'
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

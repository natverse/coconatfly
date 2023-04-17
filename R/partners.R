#' Flexible function for fetching partner data across datasets
#'
#' @param ids A list of ids named by the relevant datasets
#' @param threshold return only edges with at least this many matches
#' @param partners Whether to return inputs or outputs
#' @param bind.rows Whether to bind data.frames for each dataset together,
#'   keeping only the common columns (default \code{TRUE} for convenience but
#'   note that some columns will be dropped).
#'
#' @return
#'
#' @examples
#' \dontrun{
#' cf_partners(list(flywire='DA2_lPN'))
#'
#' DA2_lPN=cf_partners(list(flywire='DA2_lPN', malecns='DA2_lPN'))
#' }
cf_partners <- function(ids, threshold=1L, partners=c("inputs", "outputs"),
                        bind.rows=TRUE) {
  partners=match.arg(partners)
  threshold <- checkmate::assert_integerish(
    threshold, lower=1L,len = 1, null.ok = F, all.missing = F)
  ids <- checkmate::assert_named(ids, type = 'unique')
  names(ids)=match_datasets(names(ids))
  stopifnot(all(names(ids) %in% cf_datasets('all')))

  res=vector(mode = 'list', length = length(ids))
  names(res)=names(ids)

  for(n in names(ids)) {
    tres=NULL
    if(n=='flywire') {
      tres=flywire_partner_summary2(ids[[n]], partners = partners, threshold = threshold)
      tres$side=toupper(substr(tres$side,1,1))
    } else if(n=='hemibrain') {
      # a bit inelegant but not sure how else to insist
      hbconn=neuprintr::neuprint_login(dataset='hemibrain:v1.2.1')
      tres=neuprintr::neuprint_connection_table(ids[[n]], partners = partners, threshold=threshold, details = TRUE, conn = hbconn)
      tres <- tres %>%
        dplyr::mutate(
          type=dplyr::case_when(
            is.na(type) ~ paste0('hb', bodyid),
            T ~ type),
          side=stringr::str_match(name, '_([LR])$')[,2],
          side=dplyr::case_when(
            is.na(side) ~ 'R',
            T ~ side))
    } else if(n=='malecns') {
      stop("malecns not yet supported!")
    }
    tres=coconat:::standardise_partner_summary(tres)
    tres$dataset=n
    res[[n]]=tres
  }
  if(isTRUE(bind.rows)) {
    if(length(res)==1) return(res[[1]])

    nn=lapply(res, names)
    commoncols=Reduce(intersect, nn[-1], init=nn[[1]])
    res=lapply(res, "[", commoncols)

    res <- do.call(function(...) rbind(..., make.row.names=FALSE), res)
  }
  res
}


# private function to match types across datasets
# @param min_datasets How many datasets a type must be in to be included in the
#   output \code{Inf} => all datasets must contain the cell type. We may want to
#   make this cleverer e.g. by providing lists of *all* cell types for the
#   different datasets so that when cell types are found in the dataset but
#   missing from the partners we can use that negative result.
# partners argument is just used to construct a warning message
match_types <- function(x, group, partners="", min_datasets=Inf) {
  stopifnot(is.data.frame(x))
  if(!is.finite(min_datasets)) min_datasets=dplyr::n_distinct(x$dataset)
  # right now we only support type as the grouping variable
  stopifnot(length(group)==1 && group=='type')
  xg <- x %>%
    dplyr::group_by(type) %>%
    dplyr::mutate(nd=dplyr::n_distinct(dataset)) %>%
    dplyr::ungroup()
  todrop <- xg %>%
    dplyr::filter(nd<min_datasets)
  message("Matching types across datasets. Dropping ",
          nrow(todrop), "/", nrow(x),
          " ", substr(partners,1,nchar(partners)-1),
          " partner types with total weight ", sum(todrop$weight), "/", sum(x$weight))
  x <- xg %>%
    dplyr::filter(nd>1) %>%
    dplyr::select(-nd)
  x
}

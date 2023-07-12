#' Flexible function for fetching partner data across datasets
#'
#' @param ids A list of ids named by the relevant datasets (see examples)
#' @param threshold return only edges with at least this many matches. 0 is an
#'   option since neuprint sometimes returns 0 weight edges.
#' @param partners Whether to return inputs or outputs
#' @param bind.rows Whether to bind data.frames for each dataset together,
#'   keeping only the common columns (default \code{TRUE} for convenience but
#'   note that some columns will be dropped).
#'
#' @return A data.frame or a named list (when \code{bind.rows=FALSE})
#' @export
#' @examples
#' \donttest{
#' da2ds=cf_partners(list(hemibrain='DA2_lPN'), threshold=5)
#' library(dplyr)
#' da2ds %>%
#'   count(type, wt=weight, sort=TRUE)
#' }
#' \dontrun{
#' cf_partners(list(flywire='DA2_lPN'))
#'
#' DA2_lPN=cf_partners(list(flywire='DA2_lPN', malecns='DA2_lPN'))
#' }
cf_partners <- function(ids, threshold=1L, partners=c("inputs", "outputs"),
                        bind.rows=TRUE) {
  partners=match.arg(partners)
  threshold <- checkmate::assert_integerish(
    threshold, lower=0L,len = 1, null.ok = F, all.missing = F)

  neuprint.chunksize=100

  if(is.character(ids))
    ids=keys2df(ids)
  if(is.data.frame(ids)) {
    ss=split(ids$id, ids$dataset)
    res=cf_partners(ss, threshold = threshold, partners = partners, bind.rows = bind.rows)
    return(res)
  }


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
      tres=neuprintr::neuprint_connection_table(ids[[n]], partners = partners, threshold=threshold, details = TRUE, conn = npconn('hemibrain'), chunk = neuprint.chunksize)
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
      tres=malecns::mcns_connection_table(ids[[n]], partners = partners, threshold=threshold, chunk = neuprint.chunksize)
      # nb the type information we care about here is for partners
      tres2=tres %>% dplyr::select(partner, type, name) %>% dplyr::rename(bodyid=partner)
      tres$type <- malecns::mcns_predict_type(tres2)
      # set the soma side either from manually reviewed data
      tres <-  tres %>%
        dplyr::mutate(side=dplyr::case_when(
          !is.na(somaSide) & somaSide!='NA' & somaSide!='' ~ somaSide,
          T ~ malecns::mcns_soma_side(., method = "instance")
        ))
    } else if (n=='fanc') {
      tres=fancr::fanc_partner_summary(ids[[n]], partners = partners, threshold = threshold)
    }
    tres=coconat:::standardise_partner_summary(tres)
    tres$dataset=n
    tres$pre_key=paste0(abbreviate_datasets(tres$dataset), ":", tres$pre_id)
    tres$post_key=paste0(abbreviate_datasets(tres$dataset), ":", tres$post_id)
    res[[n]]=tres
  }
  if(isTRUE(bind.rows)) bind_rows2(res) else res
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
    dplyr::filter(nd>=min_datasets) %>%
    dplyr::select(-nd)
  x
}


connection_table2queryids <- function(x) {
  if(is.data.frame(x)) {
    qx=attr(x, 'queryids')
    stopifnot(is.data.frame(qx))
    return(qx)
  } else {
    l=lapply(x, connection_table2queryids)
    qx=dplyr::bind_rows(l) %>% distinct(id, dataset)
  }
}

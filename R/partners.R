#' Flexible function for fetching partner data across datasets
#'
#' @details fancr and fafbseg functions have usually used a \code{>}
#' relationship for the threshold, but here (as of May 2024) it is uniformly a
#' \code{>=} relationship.
#'
#' @param ids A list of ids named by the relevant datasets (see examples) or any
#'   other input that can be processed by the \code{\link{keys}} function
#'   (including a \code{hclust} dendrogram object.)
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

  neuprint.chunksize=10000

  if(is.character(ids) || inherits(ids, 'dendrogram') || inherits(ids, 'hclust'))
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
      # nb different threshold definition here
      tres=flywire_partner_summary2(ids[[n]], partners = partners,
                                    threshold = threshold-1L)
      tres$side=toupper(substr(tres$side,1,1))
    } else if(n=='hemibrain' || n=='opticlobe') {
      # a bit inelegant but not sure how else to insist
      tres=neuprintr::neuprint_connection_table(ids[[n]], partners = partners, threshold=threshold, details = TRUE, conn = npconn(n), chunk = neuprint.chunksize)
      tres <- tres %>%
        dplyr::mutate(
          type=dplyr::case_when(
            is.na(type) ~ paste0(abbreviate_datasets(n), partner),
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
      tres=fancr::fanc_partner_summary(ids[[n]], partners = partners,
                                       threshold = threshold-1L)
    } else if (n=='banc') {
      bids=banc_ids(ids[[n]])
      tres=fancr::with_banc(fancr::fanc_partner_summary(bids, partners = partners,
                                       threshold = threshold-1L, version=banc_version()))
      partner_col=grep("_id", colnames(tres), value = T)
      # metadf=banc_meta(tres[[partner_col]])
      metadf=banc_meta()
      colnames(metadf)[[1]]=partner_col
      tres=left_join(tres, metadf, by = partner_col)
    } else if(n=='manc') {
      tres=malevnc::manc_connection_table(ids[[n]],partners = partners, threshold=threshold, chunk = neuprint.chunksize)
      tres %>% dplyr::select(partner, type, name) %>% dplyr::rename(bodyid=partner)
    }
    tres=coconat:::standardise_partner_summary(tres)
    tres$dataset=n
    tres$pre_key=keys(tres, idcol="pre_id")
    tres$post_key=keys(tres, idcol='post_id')
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
match_types <- function(x, group="type", partners="", min_datasets=Inf) {
  stopifnot(is.data.frame(x))
  if(!is.finite(min_datasets)) min_datasets=dplyr::n_distinct(x$dataset)

  xg <- x %>%
    dplyr::group_by_at(group) %>%
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


#' Summarise the connectivity of a set of neurons grouping by type
#'
#' @param rval Choose what the function will return. \code{sparse} and
#'   \code{matrix} return sparse and dense (standard) matrices, respectively.
#' @param aggregate.query Whether to aggregate all query neurons of the same
#'   type (the default) or when \code{aggregate.query=FALSE} only to aggregate
#'   the partner neurons.
#' @param normalise Whether to normalise the reported weights as a fraction of
#'   the total for each query cell type (or individual query neuron when
#'   \code{aggregate.query=TRUE}).
#' @inheritParams cf_partners
#' @return a data.frame or (sparse) matrix based on \code{rval}. The column
#'   \code{n} refers to the number of \emph{partner} neurons.
#' @export
#' @details This function currently groups by dataset, and pre and postsynaptic
#'   type. It does not currently group by side. The forms returning matrices
#'   rely on \code{coconat::\link{partner_summary2adjacency_matrix}}.
#'
#' @examples
#' \dontrun{
#' lal78in=cf_partner_summary(cf_ids("/type:LAL00[78]"), threshold=10, partners='in')
#' lal78in
#' lal78in %>%
#'   tidyr::pivot_wider(id_cols = c(type.pre,dataset),
#'     names_from = type.post, values_from = weight, values_fill = 0)
#' lal78in %>%
#'   tidyr::pivot_wider(id_cols = c(type.pre),
#'     names_from = c(type.post,dataset), values_from = weight, values_fill = 0)
#' }
#' @importFrom glue glue
#' @importFrom dplyr .data select mutate left_join group_by n_distinct summarise
#'   arrange desc
cf_partner_summary <- function(ids, threshold=1L, partners=c("inputs", "outputs"),
                               aggregate.query=TRUE, normalise=FALSE,
                               rval=c("data.frame", "sparse", "matrix")) {
  # ids=expand_ids(ids)
  partners=match.arg(partners)
  rval=match.arg(rval)
  pp=cf_partners(ids, threshold = threshold, partners = partners)
  qmeta=cf_meta(ids)

  # query and partner suffixes
  qfix=ifelse(partners=='inputs', "post", "pre")
  pfix=setdiff(c("post", "pre"), qfix)
  join_spec="key"
  names(join_spec)=paste0(qfix, "_key")
  suffix=paste0(".",c(pfix, qfix))

  gv <- if(aggregate.query) c("dataset", "type.pre", "type.post") else {
    pp <- if(qfix=='post')
      pp %>% mutate(query=post_key)
    else
      pp %>% mutate(query=pre_key)
    c("dataset", "query", "type.pre", "type.post")
  }

  pp2 <- pp %>%
    select(-dataset) %>%
    left_join(qmeta, by = join_spec, suffix=suffix) %>%
    group_by(across(all_of(gv))) %>%
    summarise(weight=sum(weight),
              npre=n_distinct(pre_key),
              npost=n_distinct(post_key),
              .groups='drop') %>%
    arrange(desc(weight))
  if(aggregate.query) {
    # make a query type
    pp2 <- pp2 %>%
      mutate(query=paste0(abbreviate_datasets(dataset),":", .data[[glue("type.{qfix}")]]))
  }
  if(normalise) {
    pp2 <- pp2 %>% group_by(query) %>% mutate(weight=weight/sum(weight)) %>% ungroup()
  }

  if(rval=='data.frame')
    return(pp2)
  pp2 %>%
    coconat::partner_summary2adjacency_matrix(
      inputcol = "query",
      outputcol = ifelse(partners=='outputs', "type.post","type.pre"),
      standardise_input = F, sparse = rval=="sparse")
}

#' Flexible function for fetching partner data across datasets
#'
#' @details fancr and fafbseg functions have usually used a \code{>}
#'   relationship for the threshold, but here (as of May 2024) it is uniformly a
#'   \code{>=} relationship.
#'
#'   \code{MoreArgs} is structured as a list with a top layer naming datasets
#'   (using the same long names as \code{\link{cf_ids}}. The second (lower)
#'   layer names the arguments that will be passed to dataset-specific functions
#'   such as \code{fafbseg::flywire_partner_summary2} and
#'   \code{malevnc::manc_connection_table}.
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
#' @param MoreArgs Additional arguments in the form of a hierarchical list
#'   (expert use; see details and examples).
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
#'
#' cf_partners(cf_ids(malecns='AVLP539'), partners = 'o', threshold=100)
#' # use MoreArgs to prefer foreign (flywire/manc) cell types for malecns
#' cf_partners(cf_ids(malecns='AVLP539'), partners = 'o', threshold=100,
#'   MoreArgs = list(malecns=list(prefer.foreign=TRUE))
#' }
cf_partners <- function(ids, threshold=1L, partners=c("inputs", "outputs"),
                        bind.rows=TRUE, MoreArgs=list()) {
  partners=match.arg(partners)
  threshold <- checkmate::assert_integerish(
    threshold, lower=0L,len = 1, null.ok = F, all.missing = F)

  neuprint.chunksize=10000

  if(is.character(ids) || inherits(ids, 'dendrogram') || inherits(ids, 'hclust'))
    ids=keys2df(ids)
  if(is.data.frame(ids)) {
    ss=split(ids$id, ids$dataset)
    res=cf_partners(ss, threshold = threshold, partners = partners,
                    bind.rows = bind.rows, MoreArgs=MoreArgs)
    return(res)
  }

  ids <- checkmate::assert_named(ids, type = 'unique')
  names(ids)=match_datasets(names(ids))
  stopifnot(all(names(ids) %in% cf_datasets('all')))

  res=vector(mode = 'list', length = length(ids))
  names(res)=names(ids)

  for(n in names(ids)) {
    tres=NULL
    if(!is.null(MoreArgs[[n]])) checkmate::assert_list(MoreArgs[[n]], names = 'named')

    PFUN=NULL
    if(n %in% cf_datasets('external')) {
      dsd=coconat:::dataset_details(n, namespace = 'coconatfly')
      PFUN=dsd[['partnerfun']]
    }
    # everyone needs these
    commonArgs=c(list(ids[[n]], partners = partners, threshold=threshold), MoreArgs[[n]])
    tres <- if(!is.null(PFUN)) {
      tres=do.call(PFUN, commonArgs)
      if(is.null(tres) || ncol(tres)<3)
        stop("External functions should return a table with at least 3 columns:\n",
        "pre/post ids and weight with optional (partner) metadata!")
      if(ncol(tres)==3 || (ncol(tres)<=5 && !"type" %in% names(tres))) {
        # assume we need to fetch partner metadata
        partner_col=grep("_id", colnames(tres), value = T)
        if(!length(partner_col)==1)
          partner_col=grep("^partner$", colnames(tres), value = T)
        if(!length(partner_col)==1)
          stop("Unable to find a unique partner column for dataset: ", n,
               "\nExternal functions should return a table with bodyid and partner cols or query and pre/post_id")
        pids=unique(tres[[partner_col]])
        metadf=cf_meta(keys(data.frame(id=pids, dataset=n)))
        metadf=metadf[setdiff(colnames(metadf), c("dataset","key"))]
        colnames(metadf)[[1]]=partner_col
        tres[[partner_col]]=coconat::id2char(tres[[partner_col]])
        left_join(tres, metadf, by = partner_col)
      } else tres
    } else if(n=='flywire') {
      # nb different threshold definition here
      do.call(.flywire_partners, commonArgs)
    } else if(n%in%c('hemibrain', 'opticlobe')) {
      do.call(.neuprint_partners,
              c(commonArgs, list( dataset=n, conn = npconn(n), chunk = neuprint.chunksize)))
    } else if(n=='malecns') {
      # different strategy as MoreArgs needs to be split into different dests
      .malecns_partners(commonArgs[1:3], ma = MoreArgs[[n]])
    } else if (n=='fanc') {
      do.call(.fanc_partners, commonArgs)
    } else if (n=='banc') {
      do.call(.banc_partners, commonArgs)
    } else if(n=='manc') {
      do.call(.manc_partners, c(commonArgs, chunk = neuprint.chunksize))
    } else if(n == 'yakubavnc') {
      do.call(.yakubavnc_partners, c(commonArgs, chunk = neuprint.chunksize))
    } else stop("There is no partner function defined for dataset: ", n)

    tres=coconat:::standardise_partner_summary(tres)
    if(nrow(tres)>0) {
      tres$dataset=n
    } else {
      tres$dataset=character()
      warning("no ", partners, " found for `", n, "` dataset.")
    }
    tres$pre_key=keys(tres, idcol="pre_id")
    tres$post_key=keys(tres, idcol='post_id')
    res[[n]]=tres
  }
  if(isTRUE(bind.rows)) {
    res=bind_rows2(res)
    # record the datasets we tried to find
    attr(res, 'datasets')=names(ids)
    res
  } else res
}

.flywire_partners <- function(ids, partners, threshold, ...) {
  tres=flywire_partner_summary2(ids, partners=partners, threshold = threshold-1L, ...)
  tres <- tres %>%
    mutate(side=toupper(substr(.data$side, 1, 1))) %>%
    rename(class="super_class")
}

.neuprint_partners <- function(ids, partners, threshold, conn, dataset=NULL, ...) {

  tres=neuprintr::neuprint_connection_table(ids, partners=partners,
                                            conn=conn,
                                            threshold=threshold,
                                            details = TRUE, ...)
  tres <- tres %>%
    dplyr::mutate(
      type=dplyr::case_when(
        is.na(type) ~ paste0(abbreviate_datasets(dataset), partner),
        T ~ type),
      side=stringr::str_match(.data$name, '_([LR])$')[,2],
      side=dplyr::case_when(
        is.na(side) ~ 'R',
        T ~ side))
}

.malecns_partners <- function(args, ma) {
  # we need to send any extra arguments to the right function
  fa1=methods::formalArgs(malecns::mcns_connection_table)[-1]
  fa2=methods::formalArgs(malecns::mcns_predict_type)[-1]
  ma1=ma[setdiff(names(ma), fa2)]
  ma2=ma[setdiff(names(ma), names(ma1))]
  tres=do.call(malecns::mcns_connection_table, c(args, ma1))
  # nb the type information we care about here is for partners
  tres2=tres %>%
    dplyr::select("partner", "type", "name") %>%
    dplyr::rename(bodyid=partner)

  tres$type <- do.call(malecns::mcns_predict_type,
                       c(list(ids=tres2), ma2))
  # set the soma side either from manually reviewed data
  tres <-  tres %>%
    dplyr::mutate(side=dplyr::case_when(
      !is.na(somaSide) & somaSide!='NA' & somaSide!='' ~ somaSide,
      T ~ malecns::mcns_soma_side(., method = "instance")
    )) |>
    rename(class=superclass)
}

.fanc_partners <- function(ids, partners, threshold, ...) {
  # FIXME allow end user to override fanc version
  tres=fancr::fanc_partner_summary(fanc_ids(ids),
                                   partners = partners,
                                   threshold = threshold-1L,
                                   version=fanc_version(), ...)
  partner_col=grep("_id", colnames(tres), value = T)
  metadf=fanc_meta()
  colnames(metadf)[[1]]=partner_col
  tres=left_join(tres, metadf, by = partner_col)
  tres
}

.banc_partners <- function(ids, partners, threshold, ...) {
  banc_error()
  tres=fancr::with_banc(fancr::fanc_partner_summary(banc_ids(ids),
                                   partners = partners,
                                   threshold = threshold-1L,
                                   version=banc_version(), ...))
  partner_col=grep("_id", colnames(tres), value = T)
  metadf=banc_meta()
  colnames(metadf)[[1]]=partner_col
  tres=left_join(tres, metadf, by = partner_col)
  tres
}

.manc_partners <- function(ids, partners, threshold, ...) {
  tres <- malevnc::manc_connection_table(ids,partners = partners,
                                 threshold=threshold, conn=npconn('manc'), ...)
  tres <- tres %>%
    mutate(side=dplyr::case_when(
      !is.na(somaSide) & somaSide!='NA' & somaSide!='' ~ substr(somaSide,1,1),
      T ~ stringr::str_match(name, "_([LRM])$")[,2]
    ))
  tres
}

.yakubavnc_partners <- function(ids, partners, threshold,
                                details = c("instance", "group", "type", "class", "somaSide", "rootSide"),
                                ...) {
  tres <- neuprintr::neuprint_connection_table(ids, partners = partners,
                                 threshold=threshold,
                                 details=details,
                                 conn=npconn('yakubavnc'), ...)
  if('somaSide' %in% colnames(tres)) {

  tres <- tres %>%
    mutate(side=dplyr::case_when(
      !is.na(somaSide) & somaSide!='NA' & somaSide!='' ~ substr(somaSide,1,1),
      T ~ stringr::str_match(name, "_([LRM])$")[,2]
    ))
  }
  tres
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
  ndatasets=dplyr::n_distinct(x$dataset)
  if(!is.finite(min_datasets))
    min_datasets=ndatasets
  else if(min_datasets<0)
    min_datasets=ndatasets+min_datasets

  xg <- x %>%
    dplyr::group_by_at(group) %>%
    dplyr::mutate(nd=dplyr::n_distinct(dataset)) %>%
    dplyr::ungroup()
  todrop <- xg %>%
    dplyr::filter(nd<min_datasets)
  if(ndatasets>1)
    message("Matching types across datasets. Keeping ",
          nrow(x) - nrow(todrop), "/", nrow(x),
          " ", substr(partners,1,nchar(partners)-1),
          " connections with total weight ", sum(x$weight) - sum(todrop$weight), "/", sum(x$weight),
          " (", round(1-sum(todrop$weight)/sum(x$weight), digits = 2)*100, "%)"
          )
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
#' @param group Name of the column to use for grouping. Defaults to type but
#'   other options could be useful e.g. class or group.
#' @inheritParams cf_partners
#' @return a data.frame or (sparse) matrix based on \code{rval}. The column
#'   \code{n} refers to the number of \emph{partner} neurons.
#' @export
#' @details This function currently groups by dataset, and pre and postsynaptic
#'   type. It does not currently group by side. The forms returning matrices
#'   rely on \code{coconat::\link[coconat]{partner_summary2adjacency_matrix}}.
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
#'   arrange desc across ungroup
cf_partner_summary <- function(ids, threshold=1L, partners=c("inputs", "outputs"),
                               aggregate.query=TRUE, normalise=FALSE,
                               group='type',
                               rval=c("data.frame", "sparse", "matrix"),
                               MoreArgs=list()) {
  # ids=expand_ids(ids)
  partners=match.arg(partners)
  rval=match.arg(rval)
  pp=cf_partners(ids, threshold = threshold, partners = partners, MoreArgs=MoreArgs)
  qmeta=cf_meta(ids)
  if(length(group)>1)
    warning("The first grouping column will be used as a master group!")
  if(!all(group %in% colnames(pp)))
    stop("Grouping column `", group, "` not present in cf_partners result!")
  if(!all(group %in% colnames(qmeta)))
    stop("Grouping column `", group, "` not present in cf_meta result for query!")

    # query and partner suffixes
  qfix=ifelse(partners=='inputs', "post", "pre")
  pfix=setdiff(c("post", "pre"), qfix)
  join_spec="key"
  names(join_spec)=paste0(qfix, "_key")
  suffix=paste0(".",c(pfix, qfix))
  group.pre=paste0(group,".pre")
  group.post=paste0(group,".post")
  gv <- if(aggregate.query) c("dataset", group.pre, group.post) else {
    pp <- if(qfix=='post')
      pp %>% mutate(query=post_key)
    else
      pp %>% mutate(query=pre_key)
    c("dataset", "query", group.pre, group.post)
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
    querygroupcol=paste0(group[1],".",qfix)
    pp2 <- pp2 %>%
      mutate(query=paste0(abbreviate_datasets(dataset),":",
                          .data[[querygroupcol]]))
  }
  if(normalise) {
    pp2 <- pp2 %>% group_by(query) %>% mutate(weight=weight/sum(weight)) %>% ungroup()
  }

  if(rval=='data.frame')
    return(pp2)
  pp2 %>%
    coconat::partner_summary2adjacency_matrix(
      inputcol = "query",
      outputcol = ifelse(partners=='outputs', group.post[1], group.pre[1]),
      standardise_input = F, sparse = rval=="sparse")
}

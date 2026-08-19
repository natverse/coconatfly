#' Flexible function for fetching partner data across datasets
#'
#' @details fancr and fafbseg functions have usually used a \code{>}
#'   relationship for the threshold, but here (as of May 2024) it is uniformly a
#'   \code{>=} relationship.
#'
#'   \code{MoreArgs} is structured as a list with a top layer naming datasets
#'   (using the same long names as \code{\link{cf_datasets}}. The second (lower)
#'   layer names the arguments that will be passed to dataset-specific functions
#'   such as \code{fafbseg::flywire_partner_summary2} and
#'   \code{malevnc::manc_connection_table}.
#'
#' @param threshold return only edges with at least this many matches. 0 is an
#'   option since neuprint sometimes returns 0 weight edges.
#' @param partners Whether to return inputs or outputs
#' @param MoreArgs Additional arguments in the form of a hierarchical list
#'   (expert use; see details and examples).
#' @param details Which neurons to enrich with metadata. Options:
#'   \itemize{
#'     \item \code{"partner"} (default): Add metadata for partner neurons only
#'     \item \code{"query"}: Add metadata for query neurons only
#'     \item \code{"both"}: Add metadata for both with \code{.pre}/\code{.post} suffixes
#'     \item \code{"neither"}: No metadata, return minimal columns for speed
#'   }
#'   Metadata can also be added later via \code{\link{cf_add_meta}}.
#'
#' @inheritParams cf_meta
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
                        bind.rows=TRUE, MoreArgs=list(), keep.all=FALSE,
                        details=c("partner", "query", "both", "neither"),
                        use_superclass=getOption("coconatfly.use_superclass", FALSE),
                        harmonise_class=getOption("coconatfly.harmonise_class", FALSE)) {
  partners=match.arg(partners)
  details=match.arg(details)
  threshold <- checkmate::assert_integerish(
    threshold, lower=0L,len = 1, null.ok = F, all.missing = F)

  neuprint.chunksize=10000L

  if(is.character(ids) || inherits(ids, 'dendrogram') || inherits(ids, 'hclust'))
    ids=keys2df(ids)
  if(is.data.frame(ids)) {
    ss=split(ids$id, ids$dataset)
    res=cf_partners(ss, threshold = threshold, partners = partners,
                    bind.rows = bind.rows, MoreArgs=MoreArgs, details=details,
                    use_superclass=use_superclass, harmonise_class=harmonise_class)
    return(res)
  }

  ids <- checkmate::assert_named(ids, type = 'unique')
  names(ids)=match_datasets(names(ids))
  stopifnot(all(names(ids) %in% cf_datasets('all')))

  # Check that IDs have been expanded (queries resolved)
  check_expanded(ids)

  res=vector(mode = 'list', length = length(ids))
  names(res)=names(ids)

  for(n in names(ids)) {
    tres=NULL
    if(!is.null(MoreArgs[[n]])) checkmate::assert_list(MoreArgs[[n]], names = 'named')

    dsd=coconat:::dataset_details(n, namespace = 'coconatfly')
    PFUN=dsd[['partnerfun']]
    if(is.null(PFUN))
      stop("No partner function registered for dataset: ", n)

    # everyone needs these
    commonArgs=c(list(ids[[n]], partners = partners, threshold=threshold), MoreArgs[[n]])

    # Add chunk parameter for neuprint-based datasets
    if(n %in% c('hemibrain', 'opticlobe', 'yakubavnc', 'manc')) {
      commonArgs=c(commonArgs, list(chunk=neuprint.chunksize))
    }

    # malecns has a special signature (args, ma) for MoreArgs handling
    tres <- if(n=='malecns') {
      .malecns_partners(commonArgs[1:3], ma = MoreArgs[[n]])
    } else {
      do.call(PFUN, commonArgs)
    }

    tres=coconat:::standardise_partner_summary(tres)
    if(nrow(tres)>0) {
      tres$dataset=n
      tres$tissue=dataset_tissue(n)
      tres$sex=dataset_sex(n)
    } else {
      tres$dataset=character()
      tres$tissue=character()
      tres$sex=character()
      warning("no ", partners, " found for `", n, "` dataset.")
    }
    # Add keys before metadata enrichment
    tres$pre_key=keys(tres, idcol="pre_id")
    tres$post_key=keys(tres, idcol='post_id')

    # Enrich with metadata based on details option
    # Always use cf_add_meta rather than relying on partnerfun metadata
    if (details != "neither" && nrow(tres) > 0) {
      # Keep only core connectivity columns, drop all partnerfun metadata
      # cf_add_meta will re-add metadata from cf_meta for consistency
      core_cols <- c("pre_id", "post_id", "weight", "dataset", "tissue", "sex",
                     "pre_key", "post_key")
      tres <- tres[, intersect(names(tres), core_cols), drop = FALSE]

      # Determine which key columns to enrich
      query_col <- if (partners == "outputs") "pre_key" else "post_key"
      partner_col <- if (partners == "outputs") "post_key" else "pre_key"

      keycols <- switch(details,
        partner = partner_col,
        query = query_col,
        both = c("pre_key", "post_key")
      )
      suffixes <- switch(details,
        partner = "",
        query = "",
        both = c(".pre", ".post")
      )

      tres <- cf_add_meta(tres, keycol = keycols, suffix = suffixes,
                          harmonise_class = harmonise_class)
    }
    res[[n]]=tres
  }
  if(isTRUE(bind.rows)) {
    res=bind_rows2(res, keep.all = keep.all)
    # record the datasets we tried to find
    attr(res, 'datasets')=names(ids)
  }
  if (isTRUE(use_superclass)) {
    res <- rename_to_superclass(res)
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
                               MoreArgs=list(),
                               use_superclass=getOption("coconatfly.use_superclass", FALSE)) {
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

  if(rval=='data.frame') {
    if (isTRUE(use_superclass)) {
      pp2 <- rename_to_superclass(pp2)
    }
    return(pp2)
  }
  pp2 %>%
    coconat::partner_summary2adjacency_matrix(
      inputcol = "query",
      outputcol = ifelse(partners=='outputs', group.post[1], group.pre[1]),
      standardise_input = F, sparse = rval=="sparse")
}

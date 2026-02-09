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
#'   note that some columns will be dropped by unless \code{keep.all=TRUE}).
#' @param keep.all Whether to keep all columns when processing multiple datasets
#'   rather than just those in common (default=\code{FALSE} only keeps shared
#'   columns).
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
                        bind.rows=TRUE, MoreArgs=list(), keep.all=FALSE) {
  partners=match.arg(partners)
  threshold <- checkmate::assert_integerish(
    threshold, lower=0L,len = 1, null.ok = F, all.missing = F)

  neuprint.chunksize=10000L

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

    # Enrich with partner metadata if partnerfun returned minimal data
    tres <- add_partner_metadata(tres, dataset = n, partners = partners)

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
    res=bind_rows2(res, keep.all = keep.all)
    # record the datasets we tried to find
    attr(res, 'datasets')=names(ids)
    res
  } else res
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


# Add metadata to partner results if the partnerfun returned minimal columns
# This allows partnerfuns to return just ids + weight and have metadata added
# automatically via cf_meta()
add_partner_metadata <- function(tres, dataset, partners) {
  if (is.null(tres) || !is.data.frame(tres) || nrow(tres) == 0) {
    return(tres)
  }

  # Check if we need to fetch partner metadata:
  # - 3 columns (pre_id, post_id, weight) always needs enrichment
  # - <=5 columns without 'type' likely needs enrichment
  needs_enrichment <- ncol(tres) == 3 ||
    (ncol(tres) <= 5 && !"type" %in% names(tres))

  if (!needs_enrichment) {
    return(tres)
  }

  # Find the partner column based on query direction
  # For outputs: partners are post_id; for inputs: partners are pre_id
  partner_col <- if (partners == "outputs") "post_id" else "pre_id"

  # Check if expected column exists, otherwise try to find it

  if (!partner_col %in% colnames(tres)) {
    # Fallback: look for a single *_id column or 'partner' column
    id_cols <- grep("_id$", colnames(tres), value = TRUE)
    if (length(id_cols) == 1) {
      partner_col <- id_cols
    } else {
      partner_col <- grep("^partner$", colnames(tres), value = TRUE)
    }
  }

  if (length(partner_col) != 1 || !partner_col %in% colnames(tres)) {
    warning("Unable to find partner column for dataset: ", dataset,
            "\nPartnerfuns should return pre_id/post_id columns or include metadata")
    return(tres)
  }

  # Fetch metadata for unique partner IDs
  pids <- unique(tres[[partner_col]])
  # Convert to character for keys() and ensure dataset uses abbreviation
  pids_char <- coconat::id2char(pids)
  metadf <- cf_meta(keys(data.frame(id = pids_char, dataset = dataset)))

  if (is.null(metadf) || nrow(metadf) == 0) {
    return(tres)
  }

  # Remove columns that will be added by cf_partners later
  metadf <- metadf[setdiff(colnames(metadf), c("dataset", "key"))]

  # Rename id column to match partner column for joining
  colnames(metadf)[1] <- partner_col

  # Ensure partner column is character for joining
  tres[[partner_col]] <- coconat::id2char(tres[[partner_col]])

  dplyr::left_join(tres, metadf, by = partner_col)
}

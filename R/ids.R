id2int64 <- function(x) {
  if(bit64::is.integer64(x)) return(x)

  if(is.numeric(x)) {
    bad_doubles = x>= 2^53
    x[bad_doubles]=NA
  }
  bit64::as.integer64(x)
}


#' Interconvert between keys and ids/datasets
#'
#' @description Neurons within a dataset will be identified by numeric ids but
#'   these may not be unique across datasets. Therefore to make a unique dataset
#'   we use \code{keys} of the form \code{"<dataset>:<id>"}.
#'
#'   \code{keys} either confirms/tidies up an existing set of keys or converts a
#'   \code{list} or \code{data.frame} to keys.
#'
#' @details When \code{x} is a character vector, this must be in one of two
#'   forms. \emph{Either} a vector where each element is a single key of the
#'   form \code{"<dataset>:<id>"} \emph{or} a single string containing >=1 such
#'   keys separated by white space or commas (e.g. \code{"
#'   fw:4611686018427387904, hb:12345 "}). See examples.
#'
#'   As a convenience \code{x} may also be a \code{dendrogram} or \code{hclust}
#'   object resulting from a clustering operation.
#'
#' @param x A list, dataframe, dendrogram, or character vector specifying both
#'   within dataset ids and dataset names. See details and examples especially
#'   for character vector input.
#' @param idcol optional string naming the column containing ids
#'
#' @return For \code{keys} as character vector of keys of the form
#'   \code{"<dataset>:<id>"}.
#' @export
#'
#' @examples
#' # tidying up keys copied from somewhere else ...
#' keys(" fw:4611686018427387904, hb:12345 ")
#'
#' \donttest{
#' keys(cf_ids(hemibrain=12345, flywire='4611686018427387904'))
#' }
#' @rdname keys
keys <- function(x, idcol='id') {
  if(inherits(x, 'hclust'))
    x <- x$labels
  else if(inherits(x, 'dendrogram'))
    x <- labels(x)

  if(is.list(x) && !is.data.frame(x)) {
    x=data.frame(id=unlist(x),
                 dataset=rep(abbreviate_datasets(names(x)), lengths(x)))
    names(x)[1]=idcol
  } else if(is.character(x)) {
    kk <- if(all(is_key(x))) x
    else if(is_key(x, compound = TRUE)) {
      sx = gsub("[,\\s]+", " ", x, perl = T)
      scan(text = trimws(sx), sep = " ", what = "", quiet = T)
    } else
      stop("Invalid character vector specifying keys. Should look like:",
           "'mc:12345'")
    return(kk)
  } else {
    if(!is.data.frame(x))
      stop('x must be a list with elements named by datasets or\n',
           'a data.frame with columns id and dataset or \n',
           'a character vector already specifying keys.'
           )
    x$dataset=abbreviate_datasets(x$dataset)
  }
  paste0(x[['dataset']],":", x[[idcol]])
}

is_key <- function(x, compound=FALSE) {
  if(compound)
    length(x) == 1 &&
      is.character(x) &&
      !grepl("http", x) &&
      grepl("^\\s*([a-z:]+[0-9,\\s]+)+$", x, perl = T)
  else
    is.character(x) & grepl("^[a-z0-9]+:[0-9]{5,20}$", x)
}

#' Specify ids for fly connectome datasets
#'
#' @param query A query (e.g. cell type name or regular expression)
#' @param datasets Character vector naming datasets to which the \code{query}
#'   should be applied.
#' @param expand Whether to expand any queries into the matching ids (this will
#'   involve one or more calls to corresponding servers). Default \code{FALSE}.
#' @param keys Whether to turn the ids into keys \code{hb:12345} right away.
#'   Default \code{FALSE} but you may find this useful e.g. for combining lists
#'   of neurons (see examples).
#' @param hemibrain Pass hemibrain specific query or ids to this argument
#' @param flywire Pass flywire specific query or ids to this argument
#' @param malecns Pass malecns specific query or ids to this argument
#' @param manc Pass manc specific query or ids to this argument
#' @param opticlobe Pass opticlobe specific query or ids to this argument
#' @param fanc Pass fanc ids to this argument (at present we do not support
#'   metadata queries for fanc)
#' @param banc Pass banc ids to this argument (we only support basic metadata
#'   queries for banc)
#'
#' @details all neuprint datasets (hemibrain, malevnc, opticlobe, malecns) use
#'   the same query syntax although some fields may be dataset specific (see
#'   examples).
#'
#' @return A list of ids with additional class \code{cidlist}
#' @export
#' @family ids
#' @examples
#' \donttest{
#' cf_ids("DA2_lPN", datasets='brain')
#' # / introduces a regular expression
#' cf_ids("/MBON.+", datasets='brain')
#'
#' # expand query into actual ids
#' cf_ids("/type:MBON.+", datasets='brain', expand=TRUE)
#'
#' # return keys directly
#' cf_ids("/type:MBON.+", keys=TRUE)
#' # one way of combining separate lists of neurons
#' hbids=c(264083994, 5813022274)
#' c(cf_ids("/type:MBON1.+", keys=TRUE), cf_ids(hemibrain = hbids, keys = TRUE))
#'
#' # now equivalent to
#' keys(c(cf_ids("/type:MBON1.+"), cf_ids(hemibrain = hbids)))
#' }
cf_ids <- function(
    query=NULL,
    datasets=c("brain", "vnc", "hemibrain", "flywire", "malecns", "manc", "fanc",
               "opticlobe", "banc"),
    expand=FALSE,
    keys=FALSE,
    hemibrain=NULL, flywire=NULL, malecns=NULL, manc=NULL, fanc=NULL,
    opticlobe=NULL, banc=NULL) {

  nds=sum(
    !is.null(hemibrain),
    !is.null(flywire),
    !is.null(malecns),
    !is.null(manc),
    !is.null(fanc),
    !is.null(opticlobe),
    !is.null(banc)
    )
  res <- if(!is.null(query)) {
    if(nds>0)
      warning("ignoring explicit dataset arguments")
    if(missing(datasets))
      datasets=datasets[1]
    datasets=match.arg(datasets, several.ok = T)

    if('brain' %in% datasets)
      datasets=union(datasets[datasets!='brain'], c("hemibrain", "flywire", "malecns", "banc"))
    if('vnc' %in% datasets)
      datasets=union(datasets[datasets!='vnc'], c("manc", "fanc"))
    datasets=unique(datasets)
    structure(as.list(rep(query, length(datasets))), .Names=datasets)
  } else {
    if(nds==0)
      stop("You must supply either the `query` argument or one of hemibrain:opticlobe!")
    l=list(hemibrain=hemibrain, flywire=flywire, malecns=malecns, manc=manc,
           fanc=fanc, opticlobe=opticlobe, banc=banc)
    # drop any empty datasets
    l[lengths(l)>0]
  }
  if(isTRUE(expand) || isTRUE(keys)) {
    res=mapply(expand_ids, ids=res, dataset=names(res), SIMPLIFY = FALSE)
  }
  res=res[sort(names(res))]
  class(res)=union('cidlist', class(res))
  if(isTRUE(keys)) keys(res) else res
}

#' @export
#' @param ... One or more lists generated by \code{cf_ids} that should be joined
#'   together
#' @param unique Whether to remove duplicated ids when combining cf_id lists
#'   with \code{c.cidlist}
#' @rdname cf_ids
#' @examples
#' c(cf_ids(flywire = 1, hemibrain = 2), cf_ids(hemibrain = 1:2, flywire=1:2))
#' c(cf_ids(flywire = 1, hemibrain = 2), cf_ids(hemibrain = 1:2, flywire=1:2), unique=FALSE)
#' \donttest{
#' c(cf_ids(flywire = 1, hemibrain = 2), cf_ids(hemibrain = '/SMP55[0-9]', flywire=1:2))
#' }
c.cidlist <- function(..., unique=TRUE) {
  apl=list(...)
  apl=apl[lengths(apl)>0]
  if(length(apl)<1) return(list())
  # expand all of them
  apl=lapply(apl, expand_ids)
  nn=unique(unlist(sapply(apl, names), use.names = F))
  res=list()
  for(n in sort(nn)) {
    ul=unlist(lapply(apl, "[[", n), use.names = F)
    if(unique) ul=unique(ul)
    res[[n]]=ul
  }
  class(res)=union('cidlist', class(res))
  res
}

#' @export
print.cidlist <- function(x, ..., truncate=10) {
  have_cli=requireNamespace('cli', quietly = TRUE)
  for(n in names(x)) {
    ids=as.character(x[[n]])
    nids=length(ids)
    if(!isFALSE(truncate) && nids>truncate)
      ids=c(ids[seq_len(truncate)], "...")
    nidstr=paste0("[",nids," ids]")
    if(have_cli) {
      n=cli::col_blue(n)
      nidstr=cli::col_grey(nidstr)
    }
    cat(n, " ", nidstr, ": ", sep = "", paste(ids, collapse=" "), "\n")
  }
  invisible(x)
}

# private function to expand queries into the corresponding ids
expand_ids <- function(ids, dataset) {
  if(is.list(ids)) {
    ids=mapply(expand_ids, ids=ids, dataset=names(ids), SIMPLIFY = FALSE)
    return(ids)
  }
  dataset=match_datasets(dataset)
  FUN <- switch(dataset,
    manc=malevnc::manc_ids,
    fanc=I,
    malecns=malecns::mcns_ids,
    flywire=function(ids) fafbseg::flywire_ids(ids, version=fafbseg::flywire_connectome_data_version()),
    function(ids) neuprintr::neuprint_ids(ids, conn=npconn(dataset)))
  tf=try(FUN(ids), silent = T)
  if(inherits(tf, 'try-error')) {
    warning("No valid ids in dataset:", dataset)
    NULL
  } else tf
}


#' @description \code{keys2df} produces a \code{data.frame} with columns
#'   \code{id} and \code{dataset} describing the ids for each dataset. The
#'   ordering of the data.frame will match the order of keys in the input
#'   vector.
#'
#' @rdname keys
#' @export
keys2df <- function(keys, integer64=FALSE) {
  keys=keys(keys)
  res=stringr::str_match(keys, "^([a-z]{2}):([0-9]+)")
  data.frame(id=res[,3], dataset=lengthen_datasets(res[,2]))
}

#' @description \code{keys2list} converts a character vector of keys to a list of ids with one list element for each dataset
#'
#' @param keys A character vector of keys
#' @param integer64 Whether the output ids should be character vectors (the
#'   default) or \code{integer64}
#'
#' @rdname keys
#' @family ids
#' @export
#' @examples
#' \donttest{
#'
#' }
keys2list <- function(keys, integer64=FALSE) {
  iddf=keys2df(keys)
  bs=base::split(iddf[['id']],f = iddf[['dataset']])
  if(isTRUE(integer64)) {
    bs=sapply(bs, id2int64, simplify = F, USE.NAMES = T)
  }
  bs
}

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
#' these may not be unique across datasets. Therefore to make a unique datatset
#' we use \code{keys} of the form \code{"<dataset>:<id>"}.
#'
#' @param x A list or dataframe specifying both within dataset ids and dataset
#'   names.
#'
#' @return For \code{keys} as character vector of keys of the form
#'   \code{"<dataset>:<id>"}.
#' @export
#'
#' @examples
#' \donttest{
#' keys(list(hemibrain=12345, flywire='4611686018427387904'))
#' }
#' @rdname keys
keys <- function(x) {
  if(is.list(x) && !is.data.frame(x)) {
    x=data.frame(id=unlist(x),
                 dataset=rep(abbreviate_datasets(names(x)), lengths(x)))
  } else {
    if(!is.data.frame(x))
      stop('x must be a list with elements named by datasets or\n',
           'a data.frame with columns id and dataset')
    x$dataset=abbreviate_datasets(x$dataset)
  }
  glue::glue("{dataset}:{id}", .envir = x)
}

#' Specify ids for fly connectome datasets
#'
#' @param query A query (e.g. cell type name or regular expression)
#' @param datasets Character vector naming datasets to which the \code{query}
#'   should be applied.
#' @param expand Whether to expand any queries into the matching ids (this will
#'   involve one or more calls to corresponding servers). Default \code{FALSE}.
#' @param hemibrain Pass hemibrain specific query or ids to this argument
#' @param flywire Pass flywire specific query or ids to this argument
#' @param malecns Pass malecns specific query or ids to this argument
#' @param manc Pass manc specific query or ids to this argument
#' @param fanc Pass fanc ids to this argument (at present we do not support
#'   metadata queries for fanc)
#'
#' @return A list of ids
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
#' }
cf_ids <- function(
    query=NULL,
    datasets=c("brain", "vnc", "hemibrain", "flywire", "malecns", "manc", "fanc"),
    expand=FALSE,
    hemibrain=NULL, flywire=NULL, malecns=NULL, manc=NULL, fanc=NULL) {
  nds=sum(
    !is.null(hemibrain),
    !is.null(flywire),
    !is.null(malecns),
    !is.null(manc),
    !is.null(fanc)
    )
  res <- if(!is.null(query)) {
    if(nds>0)
      warning("ignoring explicit dataset arguments")
    if(missing(datasets))
      datasets=datasets[1]
    datasets=match.arg(datasets, several.ok = T)

    if('brain' %in% datasets)
      datasets=union(datasets[datasets!='brain'], c("hemibrain", "flywire", "malecns"))
    if('vnc' %in% datasets)
      datasets=union(datasets[datasets!='vnc'], c("manc", "fanc"))
    datasets=unique(datasets)
    structure(as.list(rep(query, length(datasets))), .Names=datasets)
  } else {
    if(nds==0)
      stop("You must supply either the `query` argument or one of hemibrain:fanc!")
    l=list(hemibrain=hemibrain, flywire=flywire, malecns=malecns, manc=manc, fanc=fanc)
    # drop any empty datasets
    l[lengths(l)>0]
  }
  if(isTRUE(expand)) {
    res=mapply(expand_ids, ids=res, dataset=names(res), SIMPLIFY = FALSE)
  }
  res
}

# private function to expand queries into the corresponding ids
expand_ids <- function(ids, dataset) {
  dataset=match_datasets(dataset)
  FUN <- switch(dataset,
    manc=malevnc::manc_ids,
    fanc=I,
    hemibrain=function(ids) neuprintr::neuprint_ids(ids, conn=npconn(dataset)),
    malecns=malecns::mcns_ids,
    flywire=function(ids) fafbseg::flywire_ids(ids, version=fafbseg::flywire_connectome_data_version()))
  FUN(ids)
}


#' @description \code{keys2df} produces a \code{data.frame} with columns
#'   \code{id} and \code{dataset} describing the ids for each dataset. The
#'   ordering of the data.frame will match the order of keys in the input
#'   vector.
#'
#' @rdname keys
#' @export
keys2df <- function(keys, integer64=FALSE) {
  looks_like_idvec=grepl("^[a-z0-9]+:[0-9]{5,20}", keys)
  if(!all(looks_like_idvec)) stop("Expecting keys of the form: `<dataset>:<id>`")
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

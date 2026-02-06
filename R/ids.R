id2int64 <- function(x) {
  if(bit64::is.integer64(x)) return(x)

  if(is.numeric(x)) {
    bad_doubles = x>= 2^53
    x[bad_doubles]=NA
  }
  bit64::as.integer64(x)
}

# extract numeric ids but pass on other character vectors such as queries
# always returns bit64 so results are easy to spot
extract_ids <- function(x) {
  if(is.character(x) && length(x)==1 && !fafbseg:::valid_id(x, na.ok = T) && !grepl("http", x) && grepl("^\\s*(([a-z:]{1,3}){0,1}[0-9,\\s]+)+$",x, perl=T)) {
    sx=gsub("[a-z:,\\s]+"," ", x, perl = T)
    x=scan(text = trimws(sx), sep = ' ', what = '', quiet = T)
    x <- id2int64(x)
  }
  if(is.numeric(x) || is.integer(x)) {
    x <- id2int64(x)
  }
  x
}

#' Interconvert between keys and ids/datasets
#'
#' @description Neurons within a dataset will be identified by numeric ids but
#'   these may not be unique across datasets. Therefore to make identifiers that
#'   are unique across dataset we use \code{keys} of the form
#'   \code{"<dataset>:<id>"}.
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
#' @param x A list, data frame, dendrogram, or character vector specifying both
#'   within dataset ids and dataset names. See details and examples especially
#'   for character vector input.
#' @param idcol optional string naming the column containing ids
#'
#' @return For \code{keys} a character vector of keys of the form
#'   \code{"<dataset>:<id>"}.
#' @export
#'
#' @examples
#' # tidying up keys copied from somewhere else ...
#' keys(" fw:4611686018427387904, hb:12345 ")
#'
#' \donttest{
#' keys(cf_ids(hemibrain=12345, flywire='4611686018427387904'))
#'
#' # NB this runs the query for hemibrain type MBON01 and then maps ids -> keys
#' keys(cf_ids(hemibrain='MBON01'))
#' }
#' @rdname keys
keys <- function(x, idcol='id') {
  if(inherits(x, 'hclust'))
    x <- x$labels
  else if(inherits(x, 'dendrogram'))
    x <- labels(x)

  # expand an id list
  if(inherits(x, 'cidlist'))
    x=c(x,NULL)
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
  if(nrow(x)>0)
    paste0(x[['dataset']],":", x[[idcol]])
  else
    character()
}

is_key <- function(x, compound=FALSE) {
  if(compound)
    length(x) == 1 &&
      is.character(x) &&
      !grepl("http", x) &&
      grepl("^\\s*([a-z:]+[0-9,\\s]+)+$", x, perl = T)
  else
    is.character(x) & grepl("^[a-z0-9]+:([0-9]{5,20}|0)$", x)
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
#' @param yakubavnc Pass yakuba VNC specific query or ids to this argument
#' @param banc Pass banc ids to this argument (we only support basic metadata
#'   queries for banc)
#' @param ... Queries or ids for additional named datasets (in the style of
#'   \code{hemibrain}). See \emph{Additional datasets} section.
#'
#' @section Additional datasets: coconatfly allows can be extended by
#'   registering additional datasets using the
#'   \code{coconat::\link[coconat]{register_dataset}} function. You can use
#'   \code{cf_ids} to support these additional datasets by passing additional
#'   named arguments. The only inconvenience is that these will not be available
#'   for command completion by your editor.
#'
#' @details You will often want to perform a query, most commonly for a cell
#'   \emph{type} or cell \emph{class}, rather than specific numeric ids. The
#'   most flexible way to do this is to use a regular expression (regex) query,
#'   specified with an initial \code{"/"}.
#'
#'   All neuprint datasets (hemibrain, malevnc, opticlobe, malecns) use the same
#'   query syntax although some fields may be dataset specific (see examples).
#'   The regex syntax for CAVE datasets (flywire, fanc, banc) should be the same
#'   although you may find some wrinkles because the underlying data stores are
#'   different. Note that we do not yet translate all the different fields
#'   across datasets for queries, although this is a goal. For example the
#'   neuprint/fanc/banc \code{class} field is equivalent to flywire
#'   \code{super_class}. Similarly the values are not guaranteed to be the same.
#'   Where flywire uses \code{super_class=="descending")} the manc uses
#'   \code{class=="descending neuron")}.
#'
#'   Therefore to find all DNs in these two datasets you will need to do:
#'   \code{cf_ids(manc='/class:descending.*',
#'   flywire='/super_class:descending.*')}
#'
#'   Feel free to \href{https://github.com/natverse/coconatfly/issues}{make an
#'   issue} if you find something that doesn't feel right or can suggest an improvement.
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
#'
#' # queries on classes respecting dataset idiosyncrasies
#' cf_ids(manc="/class:descend.+", flywire="/super_class:descend.+", expand = TRUE)
#' }
cf_ids <- function(
    query=NULL,
    datasets=c("brain", "vnc", "hemibrain", "flywire", "malecns", "manc", "fanc",
               "opticlobe", "banc", "yakubavnc"),
    expand=TRUE,
    keys=FALSE,
    hemibrain=NULL, flywire=NULL, malecns=NULL, manc=NULL, fanc=NULL,
    opticlobe=NULL, banc=NULL, yakubavnc=NULL, ...) {

  mc=match.call()
  cand_datasets=setdiff(names(mc), c("query", "datasets", "expand", "keys", ""))
  if(length(cand_datasets)>0) {
    dataset_args=match_datasets(cand_datasets)
  } else dataset_args=character(0L)
  nds=length(dataset_args)

  res <- if(!is.null(query)) {
    if(length(query)>1)
      stop("Sorry, `query` should be a single element character vector such as 'DNa02'")
    if(nds>0)
      warning("ignoring explicit dataset arguments")
    if(missing(datasets))
      datasets=datasets[1]
    datasets=match.arg(datasets,
                       choices = union(eval(formals()$datasets), cf_datasets()),
                       several.ok = T)

    if('brain' %in% datasets)
      datasets=union(datasets[datasets!='brain'], c("hemibrain", "flywire", "malecns", "banc"))
    if('vnc' %in% datasets)
      datasets=union(datasets[datasets!='vnc'], c("manc", "fanc", "yakubavnc"))
    datasets=unique(datasets)
    structure(as.list(rep(query, length(datasets))), .Names=datasets)
  } else {
    if(nds==0)
      stop("You must supply either the `query` argument or one of hemibrain:banc!")
    l=list(hemibrain=hemibrain, flywire=flywire, malecns=malecns, manc=manc,
           fanc=fanc, opticlobe=opticlobe, banc=banc, yakubavnc=yakubavnc)
    pl=pairlist(...)
    if(length(pl)) {
      names(pl)=match_datasets(names(pl))
      l=c(l, pl)[dataset_args]
    }

    # drop any empty datasets
    l[lengths(l)>0]
  }
  if(isTRUE(expand) || isTRUE(keys)) {
    res=mapply(expand_ids, ids=res, dataset=names(res), SIMPLIFY = FALSE)
  }
  res=res[sort(names(res))]
  class(res)=union('cidlist', class(res))
  attr(res, 'expanded') <- isTRUE(expand) || isTRUE(keys)
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
  # check all inputs are expanded
  lapply(apl, check_expanded)
  nn=unique(unlist(sapply(apl, names), use.names = F))
  res=list()
  for(n in sort(nn)) {
    ul=unlist(lapply(apl, "[[", n), use.names = F)
    if(unique) ul=unique(ul)
    res[[n]]=ul
  }
  class(res)=union('cidlist', class(res))
  attr(res, 'expanded') <- TRUE
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

# private function to select an ids function for a dataset
get_id_fun <- function(dataset) {
  dataset=match_datasets(dataset)
  FUN <- NULL
  if(dataset %in% cf_datasets('external')) {
    dsd=coconat:::dataset_details(dataset, namespace = 'coconatfly')
    FUN=dsd[['idfun']]
  }
  if(is.null(FUN) && dataset %in% cf_datasets('builtin')) {
    FUN <- switch(
      dataset,
      manc=function(ids) malevnc::manc_ids(ids, mustWork = F, conn = npconn('manc')),
      fanc=fanc_ids,
      malecns=function(ids) malecns::mcns_ids(ids, mustWork = F),
      banc=banc_ids,
      flywire=function(ids) fafbseg::flywire_ids(
        ids,
        version=fafbseg::flywire_connectome_data_version()),
      function(ids) neuprintr::neuprint_ids(ids, conn=npconn(dataset), mustWork = F))
  }
  if(is.null(FUN))
    stop("No id function for dataset ", dataset)
  FUN
}

# private function to expand queries into the corresponding ids
expand_ids <- function(ids, dataset) {
  if(is.list(ids)) {
    ids=mapply(expand_ids, ids=ids, dataset=names(ids), SIMPLIFY = FALSE)
    return(ids)
  }
  if(length(ids)==0) return(character())
  FUN=get_id_fun(dataset)
  tryCatch({
    tf=FUN(ids)
    if(length(tf)==0)
      warning("No matching ids when querying dataset:", dataset)
    tf
  }, error=function(e) {
    stop("In expand_ids: Unable to process query for dataset: `", dataset, "`. Details:\n", e,
            call.=FALSE, immediate. = TRUE)
    NULL
  })
}

# Check that IDs have been expanded (queries resolved to numeric IDs)
# This ensures all query resolution goes through cf_ids
check_expanded <- function(ids) {
  # If it's a cidlist, check the expanded attribute
  if(inherits(ids, 'cidlist')) {
    expanded <- attr(ids, 'expanded')
    if(isTRUE(expanded)) return(invisible(TRUE))
    if(isFALSE(expanded)) {
      stop("IDs contain unexpanded queries. Please use cf_ids(..., expand=TRUE) ",
           "before passing to cf_partners/cf_meta.", call. = FALSE)
    }
  }
  # For raw lists or cidlists without the attribute, check if any values look like queries
  for(n in names(ids)) {
    vals <- ids[[n]]
    # Numeric or integer64 values are OK
    if(is.numeric(vals) || bit64::is.integer64(vals)) next
    # Character values that are valid IDs are OK
    if(is.character(vals) && all(fafbseg:::valid_id(vals, na.ok = TRUE))) next
    # Otherwise it's probably a query string
    stop("IDs for dataset '", n, "' contain unexpanded queries. ",
         "Please use cf_ids(..., expand=TRUE) before passing to cf_partners/cf_meta.",
         call. = FALSE)
  }
  invisible(TRUE)
}


#' @description \code{keys2df} produces a \code{data.frame} with columns
#'   \code{id} and \code{dataset} describing the ids for each dataset. The
#'   ordering of the data.frame will match the order of keys in the input
#'   vector.
#'
#' @rdname keys
#' @export
#' @examples
#' \donttest{
#' keys2df(cf_ids('MBON01', datasets = c("hemibrain", "flywire")))
#' }
keys2df <- function(keys, integer64=FALSE) {
  keys=keys(keys)
  res=stringr::str_match(keys, "^([a-z]{2}):([0-9]+)")
  ids = if(integer64) bit64::as.integer64(res[,3]) else res[,3]
  data.frame(id=ids, dataset=lengthen_datasets(res[,2]))
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
#' keys2list(cf_ids('MBON01', datasets = c("hemibrain", "flywire")))
#' }
keys2list <- function(keys, integer64=FALSE) {
  iddf=keys2df(keys)
  bs=base::split(iddf[['id']],f = iddf[['dataset']])
  if(isTRUE(integer64)) {
    bs=sapply(bs, id2int64, simplify = F, USE.NAMES = T)
  }
  bs
}

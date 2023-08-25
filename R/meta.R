npconn <- function(dataset) {
  dataset=match_datasets(dataset)
  if(dataset=='hemibrain')
    return(neuprintr::neuprint_login(
      server="https://neuprint.janelia.org",
      dataset='hemibrain:v1.2.1'))
  else if(dataset=='malecns')
    return(malecns::mcns_neuprint())
  else if(dataset=='manc')
    return(malevnc::manc_neuprint())
  else stop("neuprint connection unavailable for dataset: ", dataset)
}

#' Fetch metadata for neurons from connectome datasets
#'
#' @details \code{MoreArgs} should be list named by the standard dataset names
#'   (e.g. as returned by \code{cf_datasets}.
#'
#' @param integer64 Whether ids should be character vectors (default) or 64 bit
#'   ints (more compact but a little fragile as they rely on the \code{bit64}
#'   extension package.)
#' @param MoreArgs A named list of arguments to be passed when fetching metadata
#'   for a given function. See details.
#'
#'   flywire_type Which metadata column to use from flytable info table
#' @inheritParams cf_partners
#'
#' @importFrom dplyr mutate rename rename_with select case_when any_of
#' @importFrom fafbseg flywire_ids
#' @export
#' @seealso \code{\link{neuprint_ids}}
#' @examples
#' \donttest{
#' da2meta=cf_meta(cf_ids(hemibrain='DA2_lPN'))
#' da2meta
#' # / introduces a regular expression
#' mbonmeta=cf_meta(cf_ids(hemibrain='/MBON.+'))
#' }
cf_meta <- function(ids, bind.rows=TRUE, integer64=FALSE,
                    MoreArgs=list(flywire=list(type=c("cell_type","hemibrain_type")))) {
  if(is.character(ids))
    ids=keys2df(ids)
  if(is.data.frame(ids)) {
    stopifnot(bind.rows)
    ss=split(ids$id, ids$dataset)
    res=cf_meta(ss, integer64 = integer64, MoreArgs = MoreArgs)
    res=res[match(keys(ids), res$key),,drop=F]
    return(res)
  }
  ids <- checkmate::assert_named(ids, type = 'unique')
  names(ids)=match_datasets(names(ids))
  stopifnot(all(names(ids) %in% cf_datasets('all')))

  res=vector(mode = 'list', length = length(ids))
  names(res)=names(ids)

  for(n in names(ids)) {
    # NB we need to use get not match.fun since the functions are not exported
    FUN=get(paste0(n, '_meta'), mode = 'function')
    args=list(ids=ids[[n]])
    args2=MoreArgs[[n]]
    if(length(args2)) args=c(args, args2)

    tres=try(do.call(FUN, args), silent = F)
    # maybe our query didn't yield anything
    if(inherits(tres, 'try-error') || is.null(tres) || !isTRUE(nrow(tres)>0))
      next
    tres$id=flywire_ids(tres$id, integer64=integer64, na_ok=TRUE)
    cols_we_want=c("id", "class", "type", 'side', 'group', "instance")
    missing_cols=setdiff(cols_we_want, colnames(tres))
    if('class' %in% missing_cols)
      tres$class=NA_character_
    if('group' %in% missing_cols)
      tres$group=bit64::as.integer64(NA)
    if('instance' %in% missing_cols) {
      tres <-if('name' %in% colnames(tres))
        tres %>% rename(instance=name)
      else
        tres %>%
        mutate(instance=case_when(
          !is.na(type) ~ paste0(type, "_", side),
          T ~ NA_character_))
    }
    tres$group=flywire_ids(tres$group, integer64 = integer64)
    missing_cols=setdiff(cols_we_want, colnames(tres))
    if(length(missing_cols)>0)
      stop("We are missing columns: ", paste(missing_cols, collapse = ','))
    tres$dataset=n
    tres$key=keys(tres)
    res[[n]]=tres
  }
  if(length(res)==0) return(NULL)
  if(bind.rows) bind_rows2(res) else res
}

flywire_meta <- function(ids, type=c("cell_type","hemibrain_type"), ...) {
  type=match.arg(type)
  tres=flytable_meta(ids,
                     version = fafbseg::flywire_connectome_data_version(),
                     unique = T, ...)
  tres <- tres %>%
    rename(id=root_id) %>%
    mutate(id=fafbseg::flywire_ids(id, integer64=T)) %>%
    mutate(side=toupper(substr(side,1,1))) %>%
    rename_with(~ sub(".+_", "", .x), .cols=any_of(type)) %>%
    rename(class=super_class)
}

hemibrain_meta <- function(ids, ...) {
  tres=neuprintr::neuprint_get_meta(ids, conn = npconn('hemibrain'), ...)
  tres <- tres %>%
    rename(id=bodyid) %>%
    mutate(side=stringr::str_match(tres$name, "_([LR])")[,2])
  tres
}

#' @importFrom dplyr .data
malecns_meta <- function(ids, ...) {
  tres=malecns::mcns_neuprint_meta(ids)
  tres <- tres %>%
    mutate(side=malecns::mcns_soma_side(.data)) %>%
    mutate(pgroup=malecns::mcns_predict_group(.data)) %>%
    mutate(ptype=malecns::mcns_predict_type(.data)) %>%
    rename(otype=type, type=ptype, ogroup=group, group=pgroup) %>%
    # special case DNs
    mutate(type=case_when(
      grepl("DN[A-z0-9_]+,", name) ~ stringr::str_match(name, "(DN[A-z0-9_]+),")[,2],
      T ~ type
    )) %>%
    rename(id=bodyid)
  tres
}

malevnc_meta <- function(ids, ...) {
  tres <- malevnc::manc_neuprint_meta(ids, ...) %>%
    mutate(side=dplyr::case_when(
      !is.na(somaSide) ~ toupper(substr(somaSide, 1, 1)),
      !is.na(rootSide) ~ toupper(substr(rootSide, 1, 1)),
      T ~ NA_character_
    )) %>%
    rename(id=bodyid)
  tres
}

fanc_meta <- function(ids, ...) {
  warning("true metadata is not currently supported for fanc!")
  data.frame(id=fancr::fanc_ids(ids), type=NA, side=NA)
}


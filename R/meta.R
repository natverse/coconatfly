npconn <- function(dataset) {
  dataset=match_datasets(dataset)
  if(dataset=='hemibrain')
    return(neuprintr::neuprint_login(dataset='hemibrain:v1.2.1'))
  else if(dataset=='malecns')
    return(malecns::mcns_neuprint())
  else if(dataset=='malevnc')
    return(malevnc::manc_neuprint())
  else stop("neuprint connection unavailable for dataset: ", dataset)
}

#' Fetch metadata for neurons from connectome datasets
#'
#' @param integer64 Whether ids should be character vectors (default) or 64 bit
#'   ints (more compact but a little fragile as they rely on the \code{bit64}
#'   extension package.)
#' @param flywire_type Which metadata column to use from flytable info table
#' @inheritParams cf_partners
#'
#' @importFrom dplyr mutate rename rename_with select case_when
#' @importFrom fafbseg flywire_ids
#' @export
#' @seealso \code{\link{neuprint_ids}}
#' @examples
#' \donttest{
#' da2meta=cf_meta(list(hemibrain='DA2_lPN'))
#' da2meta
#' # / introduces a regular expression
#' mbonmeta=cf_meta(list(hemibrain='/MBON.+'))
#' }
cf_meta <- function(ids, bind.rows=TRUE, integer64=FALSE, flywire_type=c("cell_type","hemibrain_type")) {
  flywire_type=match.arg(flywire_type)
  if(is.character(ids))
    ids=keys2df(ids)
  if(is.data.frame(ids)) {
    stopifnot(bind.rows)
    ss=split(ids$id, ids$dataset)
    res=cf_meta(ss, integer64 = integer64, flywire_type = flywire_type)
    keys=glue::glue('{dataset}:{id}', .envir = res)
    ids$key=glue::glue('{dataset}:{id}', .envir = ids)
    res=res[match(ids$key, keys),,drop=F]
    return(res)
  }
  ids <- checkmate::assert_named(ids, type = 'unique')
  names(ids)=match_datasets(names(ids))
  stopifnot(all(names(ids) %in% cf_datasets('all')))

  res=vector(mode = 'list', length = length(ids))
  names(res)=names(ids)

  for(n in names(ids)) {
    if(n=='flywire'){
      tres=flytable_meta(ids[[n]],
                         version = fafbseg::flywire_connectome_data_version(),
                         unique = T)
      tres <- tres %>%
        rename(id=root_id) %>%
        mutate(id=fafbseg::flywire_ids(id, integer64=T)) %>%
        mutate(side=toupper(substr(side,1,1))) %>%
        rename_with(~ sub(".+_", "", .x), .cols=flywire_type) %>%
        rename(class=super_class)
    } else if(n=='hemibrain') {
      tres=neuprintr::neuprint_get_meta(ids[[n]], conn = npconn('hemibrain'))
      tres <- tres %>%
        rename(id=bodyid) %>%
        mutate(side=stringr::str_match(tres$name, "_([LR])")[,2])
    } else if(n=='malecns') {
      tres=malecns::mcns_neuprint_meta(ids[[n]])
      tres <- tres %>%
        mutate(side=malecns::mcns_soma_side(.)) %>%
        mutate(pgroup=malecns::mcns_predict_group(.)) %>%
        mutate(ptype=malecns::mcns_predict_type(.)) %>%
        rename(otype=type, type=ptype, ogroup=group, group=pgroup) %>%
        rename(id=bodyid)
    } else if(n=='malevnc'){
      tres <- malevnc::manc_neuprint_meta(ids[[n]]) %>%
        mutate(side=dplyr::case_when(
          !is.na(somaSide) ~ toupper(substr(somaSide, 1, 1)),
          !is.na(rootSide) ~ toupper(substr(rootSide, 1, 1)),
          T ~ NA_character_
        )) %>%
        rename(id=bodyid)
    } else if(n=='fanc')
      stop("metadata is not currently supported for fanc!")

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
    res[[n]]=tres
  }
  if(bind.rows) bind_rows2(res) else res
}

keys2df <- function(x) {
  looks_like_idvec=grepl("^[a-z]{2}:[0-9]{5,19}", x)
  if(!all(looks_like_idvec)) stop("Expecting keys of the form: `<dataset>:<id>`")
  res=stringr::str_match(x, "^([a-z]{2}):([0-9]+)")
  data.frame(id=res[,3], dataset=lengthen_datasets(res[,2]))
}

keys2list <- function(x) {
  iddf=keys2df(x)
  base::split(iddf[['id']],f = iddf[['dataset']])
}

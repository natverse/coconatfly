#' Flexible function for fetching partner data across datasets
#'
#' @param ids A list of ids named by the relevant datasets
#' @param threshold return only edges with at least this many matches
#' @param partners Whether to return inputs or outputs
#' @param bind.rows Whether to bind data.frames for each dataset together,
#'   keeping only the common columns (default \code{TRUE} for convenience but
#'   note that some columns will be dropped).
#'
#' @return A data.frame or a named list (when \code{bind.rows=FALSE})
#'
#' @examples
#' \dontrun{
#' cf_partners(list(flywire='DA2_lPN'))
#'
#' DA2_lPN=cf_partners(list(flywire='DA2_lPN', malecns='DA2_lPN'))
#' }
cf_partners <- function(ids, threshold=1L, partners=c("inputs", "outputs"),
                        bind.rows=TRUE) {
  partners=match.arg(partners)
  threshold <- checkmate::assert_integerish(
    threshold, lower=1L,len = 1, null.ok = F, all.missing = F)
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
      tres=neuprintr::neuprint_connection_table(ids[[n]], partners = partners, threshold=threshold, details = TRUE, conn = npconn('hemibrain'))
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
      tres=malecns::mcns_connection_table(ids[[n]], partners = partners, threshold=threshold)
      # nb the type information we care about here is for partners
      tres2=tres %>% dplyr::select(partner, type, name) %>% dplyr::rename(bodyid=partner)
      tres$type <- malecns::mcns_predict_type(tres2)
      # set the soma side either from manually reviewed data
      tres <-  tres %>%
        dplyr::mutate(side=dplyr::case_when(
          !is.na(somaSide) & somaSide!='NA' & somaSide!='' ~ somaSide,
          T ~ malecns::mcns_soma_side(., method = "instance")
        ))
    }
    tres=coconat:::standardise_partner_summary(tres)
    tres$dataset=n
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
    dplyr::filter(nd>1) %>%
    dplyr::select(-nd)
  x
}

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

#' @importFrom dplyr mutate rename rename_with select case_when
#' @importFrom fafbseg flywire_ids
cf_meta <- function(ids, bind.rows=TRUE, integer64=FALSE, flywire_type=c("cell_type","hemibrain_type")) {
  flywire_type=match.arg(flywire_type)
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

# private function to bind rows keeping common columns
bind_rows2 <- function(l) {
  if(length(l)==1) return(l[[1]])

  nn=lapply(l, names)
  commoncols=Reduce(intersect, nn[-1], init=nn[[1]])
  l=lapply(l, "[", commoncols)

  l <- do.call(function(...) rbind(..., make.row.names=FALSE), l)
  l
}

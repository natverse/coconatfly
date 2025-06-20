npconn <- function(dataset) {
  dataset=match_datasets(dataset)
  if(dataset=='hemibrain')
    return(neuprintr::neuprint_login(
      server="https://neuprint.janelia.org",
      dataset='hemibrain:v1.2.1'))
  else if(dataset=='opticlobe')
    return(neuprintr::neuprint_login(
      server="https://neuprint.janelia.org",
      dataset='optic-lobe:v1.0.1'))
  else if(dataset=='malecns')
    return(malecns::mcns_neuprint())
  else if(dataset=='manc') {
    # we have a little problem here. If someone has chosen e.g. yakuba
    # then we need to switch back to MANC
    mds=getOption("malevnc.dataset", default = 'MANC')
    if(!mds %in% c("MANC", "VNC") )
      mds='MANC'
    withr::with_options(malevnc::choose_malevnc_dataset(mds, set = F),
      return(malevnc::manc_neuprint()))
  }
  else if(dataset=='yakubavnc')
    return(malevnc::manc_neuprint(
      dataset='yakuba-vnc',
      server = 'https://neuprint-yakuba.janelia.org'))
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
#' @param keep.all When fetching metadata from different datasets, whether to
#'   keep all metadata columns rather than just those in common
#'   (default=\code{FALSE})
#'
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
cf_meta <- function(ids, bind.rows=TRUE, integer64=FALSE, keep.all=FALSE,
                    MoreArgs=list(flywire=list(type=c("cell_type","hemibrain_type")))) {
  if(is.character(ids) || inherits(ids, 'dendrogram') || inherits(ids, 'hclust'))
    ids=keys2df(ids)
  if(is.data.frame(ids)) {
    stopifnot(bind.rows)
    ss=split(ids$id, ids$dataset)
    res=cf_meta(ss, integer64 = integer64, MoreArgs = MoreArgs, keep.all=keep.all)
    res=res[match(keys(ids), res$key),,drop=F]
    return(res)
  }
  ids <- checkmate::assert_named(ids, type = 'unique')
  names(ids)=match_datasets(names(ids))
  stopifnot(all(names(ids) %in% cf_datasets('all')))

  res=vector(mode = 'list', length = length(ids))
  names(res)=names(ids)

  for(n in names(ids)) {

    FUN=NULL
    if(n %in% cf_datasets('external')) {
      dsd=coconat:::dataset_details(n, namespace = 'coconatfly')
      FUN=dsd[['metafun']]
    }
    # NB we need to use get not match.fun since the functions are not exported
    if(is.null(FUN) && n %in% cf_datasets('builtin'))
      FUN <- get(paste0(n, '_meta'), mode = 'function')
    if(is.null(FUN))
      stop("There is no metadata function defined for dataset: ", n)
    args=list(ids=ids[[n]])
    args2=MoreArgs[[n]]
    if(length(args2)) args=c(args, args2)

    tres=try(do.call(FUN, args), silent = F)
    # maybe our query didn't yield anything
    if(inherits(tres, 'try-error') || is.null(tres) || !isTRUE(nrow(tres)>0))
      next
    tres$id=flywire_ids(tres$id, integer64=integer64, na_ok=TRUE)
    cols_we_want=c("id", "class", "subclass", "type", 'side', 'group', "instance")
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
          !is.na(type) & !is.na(side) ~ paste0(type, "_", side),
          !is.na(type) ~ paste0(type, "_"),
          T ~ NA_character_))
    }
    tres <- tres |>
      mutate(instance=case_when(
        is.na(instance) & !is.na(type) & !is.na(side) ~ paste0(type, "_", side),
        is.na(instance) & !is.na(type) ~ paste0(type, "_"),
        !is.na(instance) ~ instance
      ))
    tres$group=flywire_ids(tres$group, integer64 = integer64)
    missing_cols=setdiff(cols_we_want, colnames(tres))
    if(length(missing_cols)>0)
      stop("We are missing columns: ", paste(missing_cols, collapse = ','))
    tres$dataset=n
    tres$key=keys(tres)
    res[[n]]=tres
  }
  if(length(res)==0) return(NULL)
  if(bind.rows) bind_rows2(res, keep.all=keep.all) else res
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
    rename(class=super_class, subclass=cell_class, subsubclass=cell_sub_class) %>%
    rename(lineage=ito_lee_hemilineage)
}

hemibrain_meta <- function(ids, ...) {
  tres=neuprintr::neuprint_get_meta(ids, conn = npconn('hemibrain'), ...)
  tres <- tres %>%
    rename(id=bodyid) %>%
    mutate(side=stringr::str_match(tres$name, "_([LR])")[,2]) %>%
    mutate(class=NA_character_, subclass=NA_character_, subsubclass=NA_character_) %>%
    rename(lineage=cellBodyFiber)
  tres
}

opticlobe_meta <- function(ids, ...) {
  tres=neuprintr::neuprint_get_meta(ids, conn = npconn('opticlobe'), ...)
  tres <- tres %>%
    rename(id=bodyid) %>%
    mutate(side=stringr::str_match(tres$name, "_([LR])$")[,2]) %>%
    mutate(class=NA_character_, subclass=NA_character_, subsubclass=NA_character_)
  tres
}

malecns_meta <- function(ids, ...) {
  tres=malecns::mcns_neuprint_meta(ids)
  tres <- tres %>%
    mutate(side=malecns::mcns_soma_side(.)) %>%
    mutate(side=case_when(
      is.na(side) ~ rootSide,
      T ~ side
    )) %>%
    mutate(pgroup=malecns::mcns_predict_group(.)) %>%
    mutate(ptype=malecns::mcns_predict_type(.)) %>%
    rename(otype=type, type=ptype, ogroup=group, group=pgroup) %>%
    # special case DNs
    mutate(type=case_when(
      grepl("DN[A-z0-9_]+,", name) ~ stringr::str_match(name, "(DN[A-z0-9_]+),")[,2],
      T ~ type
    )) %>%
    rename(id=bodyid) %>%
    rename(class1=superclass, class2=class, subsubclass=subclass) %>%
    rename(class=class1, subclass=class2) %>%
    mutate(lineage=case_when(
      !is.na(itoleeHl) & nzchar(itoleeHl) ~ itoleeHl,
      T ~ trumanHl
    ))
  tres
}

manc_meta <- function(ids, ...) {
  tres <- malevnc::manc_neuprint_meta(ids, conn=npconn('manc'), ...) %>%
    mutate(side=dplyr::case_when(
      !is.na(somaSide) ~ toupper(substr(somaSide, 1, 1)),
      !is.na(rootSide) ~ toupper(substr(rootSide, 1, 1)),
      T ~ NA_character_
    )) %>%
    rename(id=bodyid, lineage=hemilineage) %>%
    mutate(subsubclass=NA_character_)
  tres
}

yakubavnc_meta <- function(ids, ...) {
  tres <- malevnc::manc_neuprint_meta(ids, conn = npconn('yakubavnc'), ...)
  if(!"rootSide" %in% colnames(tres))
    tres$rootSide=NA_character_
  if(!"subclass" %in% colnames(tres))
    tres$subclass=NA_character_

  tres <- tres %>%
    mutate(side=dplyr::case_when(
      !is.na(somaSide) ~ toupper(substr(somaSide, 1, 1)),
      !is.na(rootSide) ~ toupper(substr(rootSide, 1, 1)),
      T ~ NA_character_
    )) %>%
    rename(id=bodyid, lineage=hemilineage) %>%
    mutate(subsubclass=NA_character_)
  tres
}

fanc_meta <- function(ids=NULL, ...) {
  ids=fanc_ids(ids)
  fancr::with_fanc(fancorbanc_meta(table='neuron_information', ids=ids, ...))
}

banc_meta <- function(ids=NULL, ...) {
  ids=banc_ids(ids)
  fancr::with_banc(fancorbanc_meta(table='cell_info', ids=ids, ...))
}

fancorbanc_meta <- function(table, ids=NULL, ...) {
  ol_classes=c("centrifugal", "distal medulla", "distal medulla dorsal rim area",
               "lamina intrinsic", "lamina monopolar", "lamina tangential",
               "lamina wide field", "lobula intrinsic", "lobula lobula plate tangential",
               "lobula medulla amacrine", "lobula medulla tangential",
               "lobula plate intrinsic", "medulla intrinsic",
               "medulla lobula lobula plate amacrine", "medulla lobula tangential",
               "photoreceptors", "proximal distal medulla tangential",
               "proximal medulla", "serpentine medulla", "T neuron",
               "translobula plate", "transmedullary", "transmedullary Y",
               "Y neuron")
  fid=list(tag2=c('primary class',"anterior-posterior projection pattern",
                  "neuron identity", "soma side", ol_classes))
  fid=list(fid)
  names(fid)=table
  selc=list(c("id", "tag", "tag2", "pt_root_id", 'pt_supervoxel_id'))
  names(selc)=table

  cell_infos=fafbseg::flywire_cave_query(table, filter_in_dict=fid, select_columns=selc,
                                version='latest', timetravel = T, allow_missing_lookups=T)
  metadf <- if(nrow(cell_infos)<1) {
    df=data.frame(id=character(), class=character(), type=character(), side=character())
  } else {
    cell_infos2 <- cell_infos %>%
      mutate(
        tag=sub("\n\n\n*banc-bot*","", fixed = T, tag),
        pt_root_id=as.character(pt_root_id))
    cell_infos3 <- cell_infos2 %>%
      mutate(
        tag2=case_when(
          tag2 %in% ol_classes ~ 'neuron identity',
          T ~ tag2)
      ) %>%
      arrange(pt_root_id, tag) %>%
      distinct(pt_root_id, tag2, tag, .keep_all = T) %>%
      group_by(pt_root_id, tag2) %>%
      # summarise(tag=paste0(tag, collapse=";"), .groups = 'drop')
      summarise(tag={
        if(length(tag)>1 && any(grepl("?", tag, fixed = T))) {
          # we would like to remove duplicate tags
          # that would otherwise give: DNg75;DNg75?
          usx=unique(sub("?", "", tag, fixed = T))
          if(length(usx)<length(tag))
            tag=usx
        }
        paste0(tag, collapse=";")
      }, .groups = 'drop')

    cell_infos2.ol=cell_infos2 %>% filter(tag2 %in% ol_classes)

    cell_infos4 <-   cell_infos3 %>%
      tidyr::pivot_wider(id_cols = pt_root_id,
                         names_from = tag2,
                         values_from = tag,
                         values_fill = ""
      ) %>%
      rename(id=pt_root_id, class=`primary class`, apc=`anterior-posterior projection pattern`,
             type=`neuron identity`, side=`soma side`) %>%
      mutate(class=case_when(
        id %in% cell_infos2.ol$pt_root_id ~ "optic",
        class=='sensory neuron' & grepl('scending', apc) ~ paste('sensory', apc),
        (class=="" | class=='central neuron') & apc=='ascending' ~ 'ascending',
        (class=="" | class=='central neuron') & apc=='descending' ~ 'descending',
        apc=="" & class=="" ~ '',
        apc=="" ~ class,
        T ~ paste(class, apc)
      )) %>%
      mutate(class=sub(" neuron", '', class)) %>%
      mutate(side=sub('soma on ', '', side)) %>%
      mutate(side=case_when(
        is.na(side) ~ side,
        T ~ toupper(substr(side,1,1))
      )) %>%
      select(id, class, type, side) %>%
      mutate(subclass=NA_character_) %>%
      mutate(id=as.character(id))
  }
  if(!is.null(ids))
    left_join(data.frame(id=ids), metadf, by='id')
  else
    metadf
}

banc_ids <- function(ids) {
  fancorbanc_ids(ids, dataset='banc')
}

fanc_ids <- function(ids) {
  fancorbanc_ids(ids, dataset='fanc')
}

#' @importFrom dplyr pull
fancorbanc_ids <- function(ids, dataset=c("banc", "fanc")) {
  if(is.null(ids)) return(NULL)
  dataset=match.arg(dataset)
  # extract numeric ids if possible
  ids <- extract_ids(ids)
  if(is.character(ids) && length(ids)==1 && !fafbseg:::valid_id(ids)) {
    # query
    metadf=if(dataset=="banc") banc_meta() else fanc_meta()
    if(isTRUE(ids=='all')) return(fancr::fanc_ids(metadf$id, integer64 = F))
    if(isTRUE(ids=='neurons')) {
      ids <- metadf %>%
        filter(is.na(.data$class) | .data$class!='glia') %>%
        pull(.data$id)
      return(fancr::fanc_ids(ids, integer64 = F))
    }
    if(isTRUE(substr(ids, 1, 1)=="/"))
      ids=substr(ids, 2, nchar(ids))
    else warning("All FANC/BANC queries are regex queries. ",
              "Use an initial / to suppress this warning!")
    if(!grepl(":", ids)) ids=paste0("type:", ids)
    qsplit=stringr::str_match(ids, pattern = '[/]{0,1}(.+):(.+)')
    field=qsplit[,2]
    value=qsplit[,3]
    if(!field %in% colnames(metadf)) {
      stop(glue("{dataset} queries only work with these fields: ",
           paste(colnames(metadf)[-1], collapse = ',')))
    }
    ids <- metadf %>%
      filter(grepl(value, .data[[field]])) %>%
      pull(.data$id)
  } else if(length(ids)>0) {
    # check they are valid for current materialisation
    ids <- if(dataset=="banc")
      fancr::with_banc(fafbseg::flywire_latestid(ids, version = banc_version()))
    else
      fancr::with_fanc(fafbseg::flywire_latestid(ids, version = fanc_version()))
  }
  return(fancr::fanc_ids(ids, integer64 = F))
}

banc_version <- function() {
  fancr::with_banc(fanc_version())
}

fanc_version <- function() {
  fcc=fancr::fanc_cave_client()
  ver=fcc$materialize$version
  ver
}

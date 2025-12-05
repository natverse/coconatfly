
use_fanc_flytable <- function(flytable=T) {
  options(coconatfly.fanc.flytable=flytable)
}

fanc_ids <- function(ids) {
  if(getOption("coconatfly.fanc.flytable", default = F))
     fanc_meta(ids)$id
  else
    fancorbanc_ids(ids, dataset='fanc')
}

fanc_meta <- function(ids=NULL, ...) {
  if(getOption("coconatfly.fanc.flytable", default = F))
    fanc_cfmeta(ids=ids, ...)
  else {
    ids=fanc_ids(ids)
    fancr::with_fanc(fancorbanc_meta(table='neuron_information', ids=ids, ...))
  }
}

fanc_cfmeta <- function(ids=NULL, ignore.case = F, fixed = F,
                         which=NULL,
                         version=NULL, timestamp=NULL,
                        keep.all=TRUE,
                         unique=TRUE, ...) {
  if(is.null(version))
    version=fanc_version()
  df <- fancr::with_fanc(
    fafbseg::cam_meta(ids, table = 'fanc_main', base='fanc',
                      ignore.case = ignore.case, fixed = fixed,
                      unique=unique,
                      version=version, timestamp=timestamp, ...))
  df %>%
    dplyr::rename(id=root_id) %>%
    dplyr::rename(class1=superclass, class2=class, subsubclass=subclass) %>%
    dplyr::rename(class=class1, subclass=class2) %>%
    dplyr::rename(lineage=hemilineage) %>%
    dplyr::mutate(instance=paste0(type, "_", ifelse(is.na(side), "", side)))
  if(keep.all) return(df)
  df %>%
    dplyr::select(id, supervoxel_id, side, type, group, class, subclass, subsubclass, lineage)
}


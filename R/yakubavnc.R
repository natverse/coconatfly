.yakubavnc_ids <- function(ids) {
  neuprintr::neuprint_ids(ids, conn = npconn('yakubavnc'), mustWork = FALSE)
}

.yakubavnc_meta <- function(ids, ...) {
  tres <- malevnc::manc_neuprint_meta(ids, conn = npconn('yakubavnc'), ...)
  if(!"rootSide" %in% colnames(tres))
    tres$rootSide=NA_character_
  if(!"subclass" %in% colnames(tres))
    tres$subclass=NA_character_

  tres <- tres %>%
    dplyr::mutate(side=dplyr::case_when(
      !is.na(somaSide) ~ toupper(substr(somaSide, 1, 1)),
      !is.na(rootSide) ~ toupper(substr(rootSide, 1, 1)),
      TRUE ~ NA_character_
    )) %>%
    dplyr::rename(id=bodyid, lineage=hemilineage) %>%
    dplyr::mutate(subsubclass=NA_character_)
  tres
}

.yakubavnc_partners <- function(ids, partners, threshold,
                                details = c("instance", "group", "type", "class", "somaSide", "rootSide"),
                                ...) {
  tres <- neuprintr::neuprint_connection_table(ids, partners = partners,
                                 threshold=threshold,
                                 details=details,
                                 conn=npconn('yakubavnc'), ...)
  if('somaSide' %in% colnames(tres)) {
    tres <- tres %>%
      dplyr::mutate(side=dplyr::case_when(
        !is.na(somaSide) & somaSide!='NA' & somaSide!='' ~ substr(somaSide,1,1),
        TRUE ~ stringr::str_match(name, "_([LRM])$")[,2]
      ))
  }
  tres
}

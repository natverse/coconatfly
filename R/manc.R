.manc_ids <- function(ids) {
  malevnc::manc_ids(ids, mustWork = FALSE, conn = npconn('manc'))
}

.manc_meta <- function(ids, ...) {
  tres <- malevnc::manc_neuprint_meta(ids, conn=npconn('manc'), ...) %>%
    dplyr::mutate(side=dplyr::case_when(
      !is.na(somaSide) ~ toupper(substr(somaSide, 1, 1)),
      !is.na(rootSide) ~ toupper(substr(rootSide, 1, 1)),
      TRUE ~ NA_character_
    )) %>%
    dplyr::rename(id=bodyid, lineage=hemilineage) %>%
    dplyr::mutate(subsubclass=NA_character_)
  tres
}

.manc_partners <- function(ids, partners, threshold, ...) {
  tres <- malevnc::manc_connection_table(ids, partners = partners,
                                 threshold=threshold, conn=npconn('manc'), ...)
  tres <- tres %>%
    dplyr::mutate(side=dplyr::case_when(
      !is.na(somaSide) & somaSide!='NA' & somaSide!='' ~ substr(somaSide,1,1),
      TRUE ~ stringr::str_match(name, "_([LRM])$")[,2]
    ))
  tres
}

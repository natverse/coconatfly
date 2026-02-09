.hemibrain_ids <- function(ids) {
  neuprintr::neuprint_ids(ids, conn = npconn('hemibrain'), mustWork = FALSE)
}

.hemibrain_meta <- function(ids, ...) {
  tres=neuprintr::neuprint_get_meta(ids, conn = npconn('hemibrain'), ...)
  tres <- tres %>%
    dplyr::rename(id=bodyid) %>%
    dplyr::mutate(side=stringr::str_match(tres$name, "_([LR])")[,2]) %>%
    dplyr::mutate(class=NA_character_, subclass=NA_character_, subsubclass=NA_character_) %>%
    dplyr::rename(lineage=cellBodyFiber)
  tres
}

.hemibrain_partners <- function(ids, partners, threshold, ...) {
  tres=neuprintr::neuprint_connection_table(ids, partners=partners,
                                            conn=npconn('hemibrain'),
                                            threshold=threshold,
                                            details = TRUE, ...)
  tres <- tres %>%
    dplyr::mutate(
      type=dplyr::case_when(
        is.na(type) ~ paste0(abbreviate_datasets('hemibrain'), partner),
        TRUE ~ type),
      side=stringr::str_match(.data$name, '_([LR])$')[,2],
      side=dplyr::case_when(
        is.na(side) ~ 'R',
        TRUE ~ side))
}

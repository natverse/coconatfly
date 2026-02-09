.opticlobe_ids <- function(ids) {
  neuprintr::neuprint_ids(ids, conn = npconn('opticlobe'), mustWork = FALSE)
}

.opticlobe_meta <- function(ids, ...) {
  tres=neuprintr::neuprint_get_meta(ids, conn = npconn('opticlobe'), ...)
  tres <- tres %>%
    dplyr::rename(id=bodyid) %>%
    dplyr::mutate(side=stringr::str_match(tres$name, "_([LR])$")[,2]) %>%
    dplyr::mutate(class=NA_character_, subclass=NA_character_, subsubclass=NA_character_)
  tres
}

.opticlobe_partners <- function(ids, partners, threshold, ...) {
  tres=neuprintr::neuprint_connection_table(ids, partners=partners,
                                            conn=npconn('opticlobe'),
                                            threshold=threshold,
                                            details = TRUE, ...)
  tres <- tres %>%
    dplyr::mutate(
      type=dplyr::case_when(
        is.na(type) ~ paste0(abbreviate_datasets('opticlobe'), partner),
        TRUE ~ type),
      side=stringr::str_match(.data$name, '_([LR])$')[,2],
      side=dplyr::case_when(
        is.na(side) ~ 'R',
        TRUE ~ side))
}

.flywire_ids <- function(ids) {
  fafbseg::flywire_ids(
    ids,
    version = fafbseg::flywire_connectome_data_version())
}

.flywire_meta <- function(ids, type=c("cell_type","hemibrain_type"), ...) {
  type=match.arg(type)
  tres=flytable_meta(ids,
                     version = fafbseg::flywire_connectome_data_version(),
                     unique = TRUE, ...)
  tres <- tres %>%
    dplyr::rename(id=root_id) %>%
    dplyr::mutate(id=fafbseg::flywire_ids(id, integer64=TRUE)) %>%
    dplyr::mutate(side=toupper(substr(side,1,1))) %>%
    dplyr::rename_with(~ sub(".+_", "", .x), .cols=dplyr::any_of(type)) %>%
    dplyr::rename(class=super_class, subclass=cell_class, subsubclass=cell_sub_class) %>%
    dplyr::rename(lineage=ito_lee_hemilineage)
}

.flywire_partners <- function(ids, partners, threshold, ...) {
  tres=flywire_partner_summary2(ids, partners=partners, threshold = threshold-1L, ...)
  tres <- tres %>%
    dplyr::mutate(side=toupper(substr(.data$side, 1, 1))) %>%
    dplyr::rename(class="super_class")
}

.malecns_ids <- function(ids) {
  malecns::mcns_ids(ids, mustWork = FALSE)
}

.malecns_meta <- function(ids, ...) {
  tres=malecns::mcns_neuprint_meta(ids)
  tres <- tres %>%
    dplyr::mutate(side=malecns::mcns_soma_side(.)) %>%
    dplyr::mutate(side=dplyr::case_when(
      is.na(side) ~ rootSide,
      TRUE ~ side
    )) %>%
    dplyr::mutate(pgroup=malecns::mcns_predict_group(.)) %>%
    dplyr::mutate(ptype=malecns::mcns_predict_type(.)) %>%
    dplyr::rename(otype=type, type=ptype, ogroup=group, group=pgroup) %>%
    # special case DNs
    dplyr::mutate(type=dplyr::case_when(
      grepl("DN[A-z0-9_]+,", name) ~ stringr::str_match(name, "(DN[A-z0-9_]+),")[,2],
      TRUE ~ type
    )) %>%
    dplyr::rename(id=bodyid) %>%
    dplyr::rename(class1=superclass, class2=class, subsubclass=subclass) %>%
    dplyr::rename(class=class1, subclass=class2) %>%
    dplyr::mutate(lineage=dplyr::case_when(
      !is.na(itoleeHl) & nzchar(itoleeHl) ~ itoleeHl,
      TRUE ~ trumanHl
    ))
  tres
}

.malecns_partners <- function(args, ma) {
  # we need to send any extra arguments to the right function
  fa1=methods::formalArgs(malecns::mcns_connection_table)[-1]
  fa2=methods::formalArgs(malecns::mcns_predict_type)[-1]
  ma1=ma[setdiff(names(ma), fa2)]
  ma2=ma[setdiff(names(ma), names(ma1))]
  tres=do.call(malecns::mcns_connection_table, c(args, ma1))
  # nb the type information we care about here is for partners
  tres2=tres %>%
    dplyr::select("partner", "type", "name") %>%
    dplyr::rename(bodyid=partner)

  tres$type <- do.call(malecns::mcns_predict_type,
                       c(list(ids=tres2), ma2))
  # set the soma side either from manually reviewed data
  tres <-  tres %>%
    dplyr::mutate(side=dplyr::case_when(
      !is.na(somaSide) & somaSide!='NA' & somaSide!='' ~ somaSide,
      TRUE ~ malecns::mcns_soma_side(., method = "instance")
    )) |>
    dplyr::rename(class=superclass)
}

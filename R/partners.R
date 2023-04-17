#' Flexible function for fetching partner data across datasets
#'
#' @param ids A list of ids named by the relevant datasets
#' @param threshold return only edges with at least this many matches
#' @param partners Whether to return inputs or outputs
#'
#' @return
#'
#' @examples
#' \dontrun{
#' cf_partners(list(flywire='DA2_lPN'))
#' }
cf_partners <- function(ids, threshold=1L, partners=c("inputs", "outputs")) {
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
      tres=neuprintr::neuprint_connection_table(ids[[n]], partners = partners, threshold=threshold, details = TRUE)
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
      stop("malecns not yet supported!")
    }
    tres=coconat:::standardise_partner_summary(tres)
    tres$dataset=n
    res[[n]]=tres
  }
  res
}

# private function to instruct users on new mechanism for banc dataset
# note that this will not be called after bancr::register_banc_coconat()
# because that will override the built-in banc_* functions
banc_error <- function() {
  bv=try(utils::packageVersion('bancr'), silent = TRUE)
  if(inherits(bv, 'try-error') || bv<'0.2.1')
    stop("To use the banc dataset please do `natmanager::install(pkgs = 'flyconnectome/bancr')` ",
         call. = FALSE)
  stop("Please run `bancr::register_banc_coconat()` to use the banc dataset",
       call. = FALSE)
}

.banc_ids <- function(ids) {
  banc_error()
  fancorbanc_ids(ids, dataset='banc')
}

.banc_meta <- function(ids=NULL, ...) {
  banc_error()
  ids=.banc_ids(ids)
  fancr::with_banc(fancorbanc_meta(table='cell_info', ids=ids, ...))
}

.banc_partners <- function(ids, partners, threshold, ...) {
  banc_error()
  tres=fancr::with_banc(fancr::fanc_partner_summary(ids,
                                   partners = partners,
                                   threshold = threshold-1L,
                                   version=banc_version(), ...))
  partner_col=grep("_id", colnames(tres), value = TRUE)
  metadf=.banc_meta()
  colnames(metadf)[[1]]=partner_col
  tres=dplyr::left_join(tres, metadf, by = partner_col)
  tres
}

banc_version <- function() {
  fancr::with_banc(fanc_version())
}

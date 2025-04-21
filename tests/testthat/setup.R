.rhubarb_meta <- function(ids=NULL) {
  df=data.frame(
    id = c("1", "2", "3"),
    side='R',
    type = c("G1_PN", "G1.2_PN", "G2_PN"),
    group = c(10001, 10002, 10004),
    instance = c("G1_PN_R", "G1.2_PN_R", "G2_PN_R"),
    class = c("central", "central", "central"),
    subclass = c("ALPN", "ALPN", "ALPN"),
    subsubclass = c("bilateral mALT PN", "bilateral mALT PN", "mALT PN"),
    lineage = c(NA, NA, "lPN"))
  if(is.null(ids)) df else {
    df[ids,,drop=FALSE]
  }
}

register_rhubarb <- function() {
  if(! 'rhubarb' %in% cf_datasets()) {
    coconat::register_dataset('rhubarb', shortname = 'rb',
                              species = 'Rheum rhabarbarum', sex='U', age='adult',
                              metafun=.rhubarb_meta,
                              namespace = 'coconatfly')
  }
  if(! 'badrhubarb' %in% cf_datasets()) {
    coconat::register_dataset('badrhubarb', shortname = 'rb',
                              species = 'Rheum rhabarbarum', sex='U', age='adult',
                              namespace = 'coconatfly')
  }
}

# withr::defer(rm(register_rhubarb), teardown_env())

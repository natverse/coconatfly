.rhubarb_meta <- function(ids=NULL) {
  df=data.frame(
    id = 1:3,
    side='R',
    type = c("G1_PN", "G1.2_PN", "G2_PN"),
    group = c(10001, 10002, 10004),
    instance = c("G1_PN_R", "G1.2_PN_R", "G2_PN_R"),
    class = c("central", "central", "central"),
    subclass = c("ALPN", "ALPN", "ALPN"),
    subsubclass = c("bilateral mALT PN", "bilateral mALT PN", "mALT PN"),
    lineage = c(NA, NA, "lPN"))
  if(is.null(ids)) df else if(is.numeric(ids)) {
    df[ids,,drop=FALSE]
  } else if(length(ids)==1 && grepl("type:", ids)) {
    query=sub("type:", "", ids)
    df=df[grepl(query, df$type),]
  } else
    stop("unsupported query")
}

register_rhubarb <- function() {
  if(! 'rhubarb' %in% cf_datasets()) {
    coconat::register_dataset('rhubarb', shortname = 'rb',
                              species = 'Rheum rhabarbarum', sex='U', age='adult',
                              metafun=.rhubarb_meta,
                              namespace = 'coconatfly')
  }
  if(! 'badrhubarb' %in% cf_datasets()) {
    coconat::register_dataset('badrhubarb', shortname = 'bb',
                              species = 'Rheum rhabarbarum', sex='U', age='adult',
                              namespace = 'coconatfly')
  }
}

# withr::defer(rm(register_rhubarb), teardown_env())

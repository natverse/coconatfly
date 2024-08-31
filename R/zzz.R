.onLoad <- function(libname, pkgname) {
  # set up experimental extra
  coconatfly.fanc_meta=getOption('coconatfly.fanc_meta')
  if(is.null(coconatfly.fanc_meta)) {
    options(coconatfly.fanc_meta=function() {
      fafbseg::flywire_sirepo_file_memo('https://github.com/flyconnectome/2023neckconnective/blob/dev/data/fanc-neckconnective-anns.tsv', read=TRUE)
    })
  }
  invisible()
}

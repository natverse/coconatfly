cf_datasets <- function(rval=c("all", 'available')) {
  rval=match.arg(rval)
  datasets=c("flywire", "malecns", 'manc', 'fanc', 'hemibrain', 'opticlobe', 'banc')
  if(rval=='all')
    datasets
  else
    stop("not yet supported")
}

match_datasets <- function(ds) {
  ds=tolower(ds)
  match.arg(ds, choices = cf_datasets('all'), several.ok = TRUE)
}


#' Abbreviate fly connectomics dataset names
#'
#' @param ds One or more dataset long names
#'
#' @return a character vector of 2 letter abbreviations
#' @export
#'
#' @examples
#' abbreviate_datasets(c("flywire", "flywire", "hemibrain"))
abbreviate_datasets <- function(ds) {
  ds=match_datasets(ds)
  abbrevlist=c(hemibrain='hb', flywire='fw', manc='mv', fanc='fv', malecns='mc',
               opticlobe='ol', banc='bc')
  unname(abbrevlist[ds])
}

lengthen_datasets <- function(ds) {
  longlist=c(hb="hemibrain", fw="flywire", mv="manc", fv="fanc", mc="malecns",
             ol='opticlobe', bc='banc')
  ds=match.arg(ds, names(longlist), several.ok = T)
  unname(longlist[ds])
}

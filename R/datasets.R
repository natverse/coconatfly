cf_datasets <- function(rval=c("all", 'available', 'builtin', 'external')) {
  rval=match.arg(rval)
  builtin_datasets=c("flywire", "malecns", 'manc', 'fanc', 'hemibrain', 'opticlobe', 'banc', 'yakubavnc')
  if(rval=='builtin')
    return(builtin_datasets)
  extra_datasets=coconat::dataset_names(namespace = 'coconatfly')
  if(rval=='external')
    return(extra_datasets)
  datasets=union(builtin_datasets, extra_datasets)
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
  if(length(ds)<1) return(ds)
  ds=match_datasets(ds)
  abbrevlist=c(hemibrain='hb', flywire='fw', manc='mv', fanc='fv', malecns='mc',
               opticlobe='ol', banc='bc', yakubavnc='yv')
  # add any extra datasets
  abbrevlist2=coconat:::dataset_shortnames('coconatfly')
  if(length(abbrevlist2)>0) {
    nn=setdiff(names(abbrevlist2), names(abbrevlist))
    abbrevlist=c(abbrevlist, abbrevlist2[nn])
  }
  unname(abbrevlist[ds])
}

lengthen_datasets <- function(ds) {
  longlist=c(hb="hemibrain", fw="flywire", mv="manc", fv="fanc", mc="malecns",
             ol='opticlobe', bc='banc', yv='yakubavnc')
  abbrevlist2=coconat:::dataset_shortnames('coconatfly')
  if(length(abbrevlist2)>0) {
    longlist2=names(abbrevlist2)
    names(longlist2)=unname(abbrevlist2)
    nn=setdiff(names(longlist2), names(longlist))
    longlist=c(longlist, longlist2[nn])
  }

  ds=match.arg(ds, names(longlist), several.ok = T)
  unname(longlist[ds])
}

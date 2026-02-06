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
  dss=cf_datasets('all')
  res=pmatch(ds, table = dss, nomatch = 0L, duplicates.ok=TRUE)
  missing_ds=ds[res==0]
  if(length(missing_ds)>0) {
    stop("unable to match dataset(s): ",
         paste(paste0('`',missing_ds,'`'), collapse = ','),
         "\n  to any of the supported datasets:\n    ",
         paste(dss, collapse = ', '))
  }
  dss[res]
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
  abbrevlist=coconat:::dataset_shortnames('coconatfly')
  unname(abbrevlist[ds])
}

lengthen_datasets <- function(ds) {
  abbrevlist=coconat:::dataset_shortnames('coconatfly')
  longlist=names(abbrevlist)
  names(longlist)=unname(abbrevlist)
  ds=match.arg(ds, names(longlist), several.ok = TRUE)
  unname(longlist[ds])
}

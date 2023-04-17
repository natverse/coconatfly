cf_datasets <- function(rval=c("all", 'available')) {
  rval=match.arg(rval)
  datasets=c("flywire", "malecns", 'manc', 'fanc', 'hemibrain')
  if(rval=='all')
    datasets
  else
    stop("not yet supported")
}

match_datasets <- function(ds) {
  ds=tolower(ds)
  match.arg(ds, cf_datasets('all'), several.ok = TRUE)
}


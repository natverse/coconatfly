# private function to bind rows keeping common columns
bind_rows2 <- function(l) {
  ll=lengths(l)
  l=l[ll>0]
  if(length(l)==0) return(NULL)
  if(length(l)==1) return(l[[1]])

  nn=lapply(l, names)
  commoncols=Reduce(intersect, nn[-1], init=nn[[1]])
  l=lapply(l, "[", commoncols)

  l <- do.call(function(...) rbind(..., make.row.names=FALSE), l)
  l
}

cf_connections <- function() {
  dslist=list()
  npds=c("hemibrain", "manc", "malecns")
  for(ds in npds) {
    res=list(installed=T)
    if(ds=='manc')
      res$installed=requireNamespace('malevnc', quietly = T)
    else if(ds=='malecns')
      res$installed=requireNamespace('malecns', quietly = T)
    if(!res$installed) {
      res=c(res, server=NA_character_, dataset=NA_character_)
      next
    }
    conn=try(npconn(ds), silent = T)
    if(inherits(conn, 'try-error'))
      conn <- list(server=NA_character_, dataset=NA_character_)
    res2=conn[c("server", "dataset")]
    names(res2)[2]='version'
    dslist[[ds]]=c(res, res2)
  }
  # flywire
  ver=fafbseg::flywire_connectome_data_version()
  dslist[['flywire']]=list(installed=T,
                           server=ifelse(is.na(ver), NA, 'local dump'),
                              version=as.character(ver))

  # fanc
  if(requireNamespace('fancr', quietly = T)) {
    furl=try({
      u=fancr::with_fanc(fafbseg:::check_cloudvolume_url(set = F))
      sub('graphene://','', u)
    })
    if(inherits(furl, 'try-error')) furl=NA_character_
    ver=tryCatch({
      fcc=fancr::fanc_cave_client()
      as.character(fcc$materialize$version)
    }, error=function(e) NA_character_)
    fres=list(installed=T,
              server=furl,
              version=ver)
  } else {
    fres=list(installed=F, server=NA_character_, version=NA_character_)
  }
  dslist[['fanc']]=fres
  dslist
  as.data.frame(dplyr::bind_rows(dslist, .id = 'dataset'))
}


#' Status report for coconatfly installation
#'
#' @return A dataframe containing status information for connectome datasets (invisibly).
#' @importFrom dplyr filter
#' @export
#'
#' @examples
#' \donttest{
#' dr_coconatfly()
#' }
dr_coconatfly <- function() {
  message("# Dataset details")
  cfc=cf_connections()
  print(cfc)
  cat("For additional information (inc auth issues) for flywire or fanc datasets, try:\n",
      '`fafbseg::dr_fafbseg()` or ',
      '`fancr::dr_fanc()`\n\n')

  if(is.na(filter(cfc, .data$dataset=='hemibrain')$server))
    cli::cli_alert_danger(paste0(
      "Cannot connect to hemibrain dataset. You probably need to set up a ",
      "neuprint token!\n",
      "See {.url https://natverse.org/coconatfly/articles/getting-started.html}"))

  if(!isTRUE(filter(cfc, .data$dataset=='manc')$installed))
    cli::cli_alert_danger(
      "To use the manc dataset do:\n{.code natmanager::install(pkgs = 'malevnc')}")
  else if(is.na(filter(cfc, .data$dataset=='manc')$server))
    cli::cli_alert_danger(paste0(
      "Cannot connect to manc dataset. You probably need to set up a ",
      "neuprint token!\n",
      "See {.url https://natverse.org/coconatfly/articles/getting-started.html}"))

  if(is.na(filter(cfc, .data$dataset=='flywire')$server))
    cli::cli_alert_danger(paste0(
      "No available data dump for the flywire dataset, try:\n",
            '{.code fafbseg::download_flywire_release_data()}'))

  if(!isTRUE(filter(cfc, .data$dataset=='fanc')$installed))
    cli::cli_alert_danger(
      "To use the fancr dataset do:\n{.code natmanager::install(pkgs = 'fancr')}")
  else if(is.na(filter(cfc, .data$dataset=='fanc')$server))
    cli::cli_alert_danger(
      "To debug connection issues to the fanc dataset, try:\n{.code fancr::dr_fanc()}")

  invisible(cfc)
}

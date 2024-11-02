# private function to bind rows keeping common columns
bind_rows2 <- function(l, keep.all=FALSE) {
  ll=lengths(l)
  l=l[ll>0]
  if(length(l)==0) return(NULL)
  if(length(l)==1) return(l[[1]])

  if(!keep.all) {
    nn=lapply(l, names)
    commoncols=Reduce(intersect, nn[-1], init=nn[[1]])
    l=lapply(l, "[", commoncols)
    l <- do.call(function(...) rbind(..., make.row.names=FALSE), l)
  } else {
    l <- dplyr::bind_rows(l)
  }
  l
}

cf_connections <- function() {
  dslist=list()
  npds=c("hemibrain", "manc", "malecns", 'opticlobe')
  for(ds in npds) {
    res=list(installed=T)
    if(ds=='manc')
      res$installed=requireNamespace('malevnc', quietly = T)
    else if(ds=='malecns')
      res$installed=requireNamespace('malecns', quietly = T)
    else if(ds=='opticlobe')
      res$installed=requireNamespace('malevnc', quietly = T)
    if(!res$installed) {
      res=c(res, server=NA_character_, dataset=NA_character_, version=NA_character_)
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
  dslist[['fanc']]=check_fanc()
  dslist[['banc']]=check_banc()
  dslist
  as.data.frame(dplyr::bind_rows(dslist, .id = 'dataset'))
}


check_fanc <- function() {
  if(requireNamespace('fancr', quietly = T)) {
    have_token=!inherits(try(fafbseg::chunkedgraph_token(), silent = TRUE), 'try-error')
    if(have_token) {
      furl=try({
        u=fancr::with_fanc(fafbseg:::check_cloudvolume_url(set = F), force = F)
        sub('graphene://','', u)
      }, silent = T)
      if(inherits(furl, 'try-error')) furl=NA_character_
      if(is.na(furl)) ver=NA_character_
      else {
        ver=try(silent = T, {
          fcc=fancr::fanc_cave_client()
          as.character(fcc$materialize$version)
        })
        if(inherits(ver, 'try-error')) ver=NA_character_
      }
    } else {
      furl=NA_character_
      ver=NA_character_
    }
    fres=list(installed=T,
              server=furl,
              version=ver)
  } else {
    fres=list(installed=F, server=NA_character_, version=NA_character_)
  }
  fres
}

check_banc <- function() {
  if(requireNamespace('fancr', quietly = T)) {
    fres=try(fancr::with_banc(check_fanc()), silent = T)
    if(inherits(fres, 'try-error'))
      fres=list(installed=T, server=NA_character_, version=NA_character_)
  } else {
    usethis::ui_info('Access to the BANC Dataset requires installation of fancr!')
    fres=list(installed=F, server=NA_character_, version=NA_character_)
  }
  fres
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

  if(!isTRUE(filter(cfc, .data$dataset=='opticlobe')$installed))
    cli::cli_alert_danger(
      "To use the opticlobe dataset do:\n{.code natmanager::install(pkgs = 'malevnc')}")
  else if(is.na(filter(cfc, .data$dataset=='opticlobe')$server))
    cli::cli_alert_danger(paste0(
      "Cannot connect to opticlobe dataset. You probably need to set up a ",
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

  if(!isTRUE(filter(cfc, .data$dataset=='banc')$installed))
    cli::cli_alert_danger(
      "To use the fancr dataset do:\n{.code natmanager::install(pkgs = 'fancr')}")
  else if(is.na(filter(cfc, .data$dataset=='banc')$server))
    cli::cli_alert_danger(
      "To debug connection issues to the banc dataset, try:\n{.code fancr::dr_fanc()}")

  # special case of most common auth issue
  have_token=!inherits(try(fafbseg::chunkedgraph_token(), silent = TRUE), 'try-error')
  if(!have_token)
    usethis::ui_info(paste0(
    'No CAVE token found. This is required to access fanc/banc datasets!\n',
    "Set one with {usethis::ui_code('fancr::fanc_set_token()')}"))

  invisible(cfc)
}

#' Find VFB ids for individual neurons
#'
#' @param keys A character vector of keys (like \code{hb:10001}) or a data frame
#'   containing \code{id} and \code{dataset} columns.
#'
#' @return A data frame containing a \code{vfb_id} column if \code{keys} was a
#'   data frame, a character vector otherwise.
#' @export
#'
#' @examples
#' \donttest{
#' dnm=cf_meta(cf_ids(manc='/type:DNa0[1-3]', hemibrain = '/type:DNa0[1-3]'))
#' cf_vfb_ids(mbonmeta)
#' # nb this function needs keys as input
#' cf_vfb_ids(cf_ids(manc='/type:DNa0[1-3]', keys=TRUE))
#' }
cf_vfb_ids <- function(keys) {
  wasdf=is.data.frame(keys)
  kdf=keys2df(keys)
  kdf$vfb_database=vfb_databases(kdf$dataset)
  kdf2 <- kdf %>%
    group_by(dataset) %>%
    mutate(vfb_id=case_when(
      is.na(vfb_database) ~ NA_character_,
      T ~ xref2vfb(id, db=vfb_database)
    ))

  if(wasdf) {
    keys$vfb_id=kdf2$vfb_id
    keys
  } else {
    kdf2$vfb_id
  }
}

check_vfbconnect <- function() {
  if(!requireNamespace('vfbconnectr')) {
    stop("Please install vfbconnectr using:\n",
         "natmanager::install(pkgs = 'vfbconnectr')")
  }
  vc=vfbconnectr::VfbConnect()
}

xref2vfb <- function(ids, db=NULL) {
  db=unique(db)
  vc=check_vfbconnect()
  res=vc$neo_query_wrapper$xref_2_vfb_id(as.list(ids),db=db)
  if(is.null(res) || length(res)==0) {
    rep(NA_character_, length(ids))
  } else {
    rdf=vfbconnectr::vc_df(res)
    if(!"vfb_id" %in% names(rdf))
      rdf$vfb_id=NA_character_
    rdf$id=names(res)
    # make sure we are in the order that we started
    lj=left_join(data.frame(id=ids), rdf, by='id')
    lj$vfb_id
  }
}

# map cf datasets to vfb "databases"
vfb_databases <- function(dataset) {
  # ds=cf_datasets()
  cfds=c("flywire", "malecns", "manc", "fanc", "hemibrain", "opticlobe")
  vfbdbs=c(
    "flywire783",
    NA_character_,
    "neuprint_JRC_Manc",
    NA_character_,
    "neuprint_JRC_Hemibrain_1point1",
    "neuprint_JRC_OpticLobe_v1_0"
  )
  names(vfbdbs)=cfds
  vfbdbs[dataset]
}


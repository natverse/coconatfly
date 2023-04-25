# private function taking a list of input and output connection tables
# and turning them into a cosine matrix
multi_cosine_matrix <- function(x, partners, nas, group='type') {
  if(is.data.frame(x)) {
    if(length(partners)>1)
      stop("If you provide a data.frame as input you must specify just one of inputs/outputs")
    x=list(x)
    names(x)=partners
  }
  # a bit of a shuffle because c(NULL, <integer64>) removes the class
  ids=unique(c(x$outputs$pre_key, x$inputs$post_key))

  cm=list()
  if('outputs' %in% partners) {
    oam <- coconat::partner_summary2adjacency_matrix(
      x[['outputs']],
      inputcol = 'pre_key',
      outputcol = group,
      inputids = ids)
    cm[['cout']] = coconat::cosine_sim(oam, transpose = T)
    cm[['wout']]=sum(x[['outputs']]$weight)
  }
  if('inputs' %in% partners) {
    iam <- coconat::partner_summary2adjacency_matrix(
      x[['inputs']],
      inputcol = group, outputcol = 'post_key', outputids = ids)
    cm[['cin']] = coconat::cosine_sim(iam, transpose = F)
    cm[['win']]=sum(x[['inputs']]$weight)
  }
  cm <- coconat::prepare_cosine_matrix(cm, partners = partners, action=nas)
  cm
}


#' Multidataset cosine clustering
#'
#' @inheritParams cf_partners
#' @inheritParams neuprintr::neuprint_cosine_plot
#'
#' @return The result of \code{\link{heatmap}} invisibly including the row and
#'   column dendrograms or when \code{heatmap=FALSE}, an \code{\link{hclust}}
#'   dendrogram
#' @export
#'
#' @examples
#' \dontrun{
#' cf_cosine_plot(list(flywire="/type:LAL.+", malecns="/type:LAL.+"))
#' }
cf_cosine_plot <- function(ids, ..., threshold=5,
                           partners = c("outputs", "inputs"),
                           labRow='{type}_{abbreviate_datasets(dataset)}{side}',
                           group='type',
                           heatmap=TRUE,
                           interactive=FALSE,
                           nas=c('zero','drop'),
                           method=c("ward.D", "single", "complete", "average",
                                    "mcquitty", "median", "centroid", "ward.D2")) {
  method=match.arg(method)
  partners=match.arg(partners, several.ok = T)
  x=multi_connection_table(ids, partners = partners, threshold = threshold, group=group)

  cm <- multi_cosine_matrix(x, partners = partners, group=group, nas=nas)

  if(is.character(labRow) && length(labRow)==1 && any(grepl("\\{", labRow))) {
    tm=cf_meta(colnames(cm))
    labRow <- glue::glue(labRow, .envir = tm)
  }
  if(interactive) {
    try(cv <- requireNamespace('coconat', versionCheck=list(op='>', version='0.1.0')))
    if(inherits(cv, 'try-error'))
      stop("Please install/update suggested package coconat.\n",
           "natmanager::install(pkgs = 'coconat')\n","is a good way to do this")
  }
  coconat:::cosine_heatmap(cm, interactive = interactive, labRow = labRow,
                           method = method, heatmap=heatmap, ...)
}

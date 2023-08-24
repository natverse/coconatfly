# private function taking a list of input and output connection tables
# and turning them into a cosine matrix
multi_cosine_matrix <- function(x, partners, nas, group='type') {
  if(is.data.frame(x)) {
    x=split(x, x$partners)
    partners=names(x)
  }
  # a bit of a shuffle because c(NULL, <integer64>) removes the class
  ids=unique(c(x$outputs$pre_key, x$inputs$post_key))

  if(isTRUE(group))
    group='type'

  cm=list()
  if('outputs' %in% partners) {
    groupcol <- if(isFALSE(group)) "post_key" else group
    oam <- coconat::partner_summary2adjacency_matrix(
      x[['outputs']],
      inputcol = 'pre_key',
      outputcol = groupcol,
      inputids = ids)
    cm[['cout']] = coconat::cosine_sim(oam, transpose = T)
    cm[['wout']]=sum(x[['outputs']]$weight)
  }
  if('inputs' %in% partners) {
    groupcol <- if(isFALSE(group)) "pre_key" else group
    iam <- coconat::partner_summary2adjacency_matrix(
      x[['inputs']],
      inputcol = groupcol, outputcol = 'post_key', outputids = ids)
    cm[['cin']] = coconat::cosine_sim(iam, transpose = F)
    cm[['win']]=sum(x[['inputs']]$weight)
  }
  cm <- coconat::prepare_cosine_matrix(cm, partners = partners, action=nas)
  cm
}


#' @importFrom dplyr distinct all_of
#' @rdname cf_cosine_plot
#' @export
#' @return \code{multi_connection_table} returns a connectivity dataframe as
#'   returned by \code{cf_partners} but with an additional column
#'   \code{partners} which indicates (for each row) whether the partner neurons
#'   are the input or output neurons.
multi_connection_table <- function(ids, partners=c("inputs", "outputs"),
                                   threshold=1L,
                                   group='type') {
  partners=match.arg(partners, several.ok = T)
  if(length(partners)>1) {
    l=sapply(partners, simplify = F, function(p)
      multi_connection_table(ids, partners=p, threshold = threshold, group=group))
    l=dplyr::bind_rows(l)
    return(l)
  }
  x <- cf_partners(ids, threshold = threshold, partners = partners)
  if(is.character(group))
    x <- match_types(x, group, partners=partners)
  # mark which column was used for the query
  x$partners=partners
  x
}

is.mct <- function(x) {
  is.data.frame(x) && all(c("pre_id", "post_id", "dataset", "partners") %in% colnames(x))
}

#' Multi dataset cosine clustering
#'
#' @details \code{group=FALSE} only makes sense for single dataset clustering -
#'   type labels are essential for linking connectivity across datasets. However
#'   \code{group=FALSE} can be useful e.g. for co-clustering columnar elements
#'   in the visual system that have closely related partners usually because
#'   they are in neighbouring columns. At the time of writing, there is no
#'   metadata support in FANC so \code{group=FALSE} is the only option there.
#'
#'   \code{group} can be set to other metadata columns such as \code{class} or
#'   \code{hemilineage}, \code{serial} (serially homologous cell group) if
#'   available. This can reveal other interesting features of organisation.
#' @param ids Either a set of ids \emph{OR} a
#' @param group The name or the grouping column for partner connectivity
#'   (defaults to \code{"type"}) or a logical where \code{group=FALSE} means no
#'   grouping (see details).
#' @param drop_dataset_prefix Whether to remove dataset prefix such as
#'   \code{hb:} or \code{fw:} from dendrograms. This is useful when reviewing
#'   neurons in interactive mode.
#' @inheritParams cf_partners
#' @inheritParams neuprintr::neuprint_cosine_plot
#'
#' @return The result of \code{\link{heatmap}} invisibly including the row and
#'   column dendrograms or when \code{heatmap=FALSE}, an \code{\link{hclust}}
#'   dendrogram
#' @export
#'
#' @examples
#' \donttest{
#' # basic cosine clustering, in this case for one dataset
#' cf_cosine_plot(cf_ids(hemibrain="/type:LAL00.+"))
#'
#' # same but dropping the dataset prefix in the column labels
#' cf_cosine_plot(cf_ids(hemibrain="/type:LAL00.+"),
#'   drop_dataset_prefix = TRUE)
#'
#' # only cluster by inputs
#' cf_cosine_plot(cf_ids(hemibrain="/type:LAL00.+"), partners='in')
#'
#' # or outputs
#' cf_cosine_plot(cf_ids(hemibrain="/type:LAL00.+"), partners='in')
#'
#' # the same but without grouping partner connectivity by type
#' # only makes sense for single dataset plots
#' cf_cosine_plot(cf_ids(hemibrain="/type:LAL00.+"), group = FALSE)
#'
#' # bigger clustering
#' lalhc=cf_cosine_plot(cf_ids(hemibrain="/type:LAL.+"), heatmap=FALSE)
#' lalmeta=cf_meta(lalhc$labels)
#' lalmeta=coconat::add_cluster_info(lalmeta, lalhc, h=0.75, idcol='key')
#'
#' }
#' \dontrun{
#' ## The previous examples are for single datasets to avoid authentication issues
#' ## on the build server, but similar queries could be run for multiple datasets
#' cf_cosine_plot(cf_ids(flywire="/type:LAL.+", malecns="/type:LAL.+"))
#'
#' cf_cosine_plot(cf_ids("/type:LAL.+", datasets='brain'))
#' # same as since the default is brain
#' cf_cosine_plot(cf_ids("/type:LAL.+"))
#'
#' # just make the hclust dendrogram
#' lalhc=cf_cosine_plot(cf_ids("/type:LAL.+"), heatmap=FALSE)
#' lalmeta=cf_meta(lalhc$labels)
#' lalmeta=coconat::add_cluster_info(lalmeta, lalhc, h=0.75)
#'
#' # look at the results interactively
#' cf_cosine_plot(cf_ids("/type:LAL.+"), interactive=TRUE)
#' }
#'
#' \donttest{
#' # Show case examples of using multi_connection_table to allow
#' # only a subset of partnets to be used for typing
#' mct=multi_connection_table(cf_ids(hemibrain="/lLN2.+"), partners='in')
#' cf_cosine_plot(mct)
#' library(dplyr)
#' mct2=mct %>% filter(!grepl("PN",type))
#' cf_cosine_plot(mct2)
#'
#' mct3=cf_ids("/type:lLN2.+", datasets=c("hemibrain", "flywire")) %>%
#'   multi_connection_table(., partners='in') %>%
#'   mutate(class=case_when(
#'    grepl("LN", type) ~ "LN",
#'    grepl("RN", type) ~ "RN",
#'    grepl("^M.*PN", type) ~ 'mPN',
#'    grepl("PN", type) ~ 'uPN',
#'    T ~ 'other'
#'   )) %>%
#'   # try merging connectivity for partners that don't have much specificity
#'   mutate(type=case_when(
#'   class=="RN" ~ sub("_.+", "", type),
#'   class=="uPN" ~ 'uPN',
#'   TRUE ~ type
#'   ))
#' \dontrun{
#' mct3%>%
#'   # remove RN/uPN connectivity could also use the merged connectivity
#'   filter(!class %in% c("RN", "uPN")) %>%
#'   cf_cosine_plot(interactive=TRUE)
#' }
#'
#' # This time focus in on a small number of query neurons
#' mct3 %>%
#'   mutate(query_key=ifelse(partners=='outputs', pre_key, post_key)) %>%
#'   filter(query_key %in% cf_ids('/type:lLN2(T_[bde]|X08)',
#'     datasets = c("hemibrain", "flywire"), keys = TRUE)) %>%
#'   cf_cosine_plot()
#' }
#'
#' # another worked example lLN1 neurons
#' \donttest{
#' lLN1=cf_ids("/type:lLN1_.+", datasets=c("hemibrain", "flywire")) %>%
#'   multi_connection_table(., partners='in') %>%
#'   mutate(class=case_when(
#'    grepl("LN", type) ~ "LN",
#'    grepl("RN", type) ~ "RN",
#'    grepl("^M.*PN", type) ~ 'mPN',
#'    grepl("PN", type) ~ 'uPN',
#'    T ~ 'other'
#'   )) %>%
#'   mutate(type=case_when(
#'   class=="RN" ~ sub("_.+", "", type),
#'   class=="uPN" ~ 'uPN',
#'   TRUE ~ type
#'   ))
#'
#' lLN1 %>%
#'   filter(!class %in% c("RN", "uPN")) %>%
#'   cf_cosine_plot()
#' }
cf_cosine_plot <- function(ids=NULL, ..., threshold=5,
                           partners = c("outputs", "inputs"),
                           labRow='{type}_{coconatfly::abbreviate_datasets(dataset)}{side}',
                           group='type',
                           heatmap=TRUE,
                           interactive=FALSE,
                           drop_dataset_prefix=FALSE,
                           nas=c('zero','drop'),
                           method=c("ward.D", "single", "complete", "average",
                                    "mcquitty", "median", "centroid", "ward.D2")) {
  method=match.arg(method)
  partners=match.arg(partners, several.ok = T)
  if(is.mct(ids)) {
    x=ids
    partners=unique(x$partners)
  } else
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
  if(drop_dataset_prefix)
    colnames(cm)=sub("^[a-z]+:","", colnames(cm))
  coconat:::cosine_heatmap(cm, interactive = interactive, labRow = labRow,
                           method = method, heatmap=heatmap, ...)
}

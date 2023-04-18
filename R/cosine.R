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
  empty_ids=bit64::integer64()
  ids=unique(c(
    if(is.null(x$outputs$pre_id)) empty_ids else x$outputs$pre_id,
    if(is.null(x$inputs$post_id)) empty_ids else x$inputs$post_id
  ))

  cm=list()
  if('outputs' %in% partners) {
    oam <- coconat::partner_summary2adjacency_matrix(
      x[['outputs']],
      inputcol = 'pre_id',
      outputcol = group,
      inputids = ids)
    cm[['cout']] = coconat::cosine_sim(oam, transpose = T)
    cm[['wout']]=sum(x[['outputs']]$weight)
  }
  if('inputs' %in% partners) {
    iam <- coconat::partner_summary2adjacency_matrix(
      x[['inputs']],
      inputcol = group, outputcol = 'post_id', outputids = ids)
    cm[['cin']] = coconat::cosine_sim(iam, transpose = F)
    cm[['win']]=sum(x[['inputs']]$weight)
  }
  cm <- coconat::prepare_cosine_matrix(cm, partners = partners, action=nas)
  cm
}

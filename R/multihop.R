# Multihop (effective) connectivity
#
# Compute effective connectivity from a set of query neurons to their n-th order
# partners, passing through intermediate interneuron layers, following the
# input-normalise-then-multiply approach of Schlegel et al. 2021
# (doi:10.7554/eLife.62576). See multihop-plan.md / cf_cosine_plot docs.

# Build a raw pre x post sparse adjacency matrix from a cf_partners table.
# Data is already standardised by cf_partners so we skip standardise_input.
.mh_adjacency <- function(tbl) {
  coconat::partner_summary2adjacency_matrix(
    tbl, inputcol = "pre_key", outputcol = "post_key",
    standardise_input = FALSE, sparse = TRUE)
}

# Effective query x target-type matrix for a SINGLE dataset and direction.
# Returns a sparse Matrix (query neurons as rows, target types as columns) or
# NULL if the walk dies out.
multihop_effective_matrix <- function(dskeys, partners, nhops, threshold,
                                      min_frac, group = "type",
                                      remove_query = FALSE, MoreArgs = list()) {
  stopifnot(nhops >= 1L)
  # one min_frac per stage: nhops intermediate prunes + 1 terminal cut
  mf <- rep_len(min_frac, nhops + 1L)
  query_keys <- unique(as.character(dskeys))
  # which side of a hop table is the advancing frontier / query anchor
  newfrontier_key <- if (partners == "outputs") "post_key" else "pre_key"

  frontier <- query_keys
  running <- NULL          # query x current-frontier (neuron resolution)
  far_types <- NULL        # named vector: terminal neuron key -> type

  for (h in seq_len(nhops + 1L)) {
    if (length(frontier) == 0) return(NULL)
    tbl <- cf_partners(frontier, threshold = threshold, partners = partners,
                       MoreArgs = MoreArgs)
    if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
    if (is.character(group) && !group %in% colnames(tbl))
      stop("Grouping column `", group, "` not present in cf_partners result!")

    Mn <- coconat::colScaleM(.mh_adjacency(tbl))
    # advance the query x frontier running matrix (see multihop-plan.md):
    # outputs -> multiply by Mn (old x new); inputs -> by t(Mn) (old x new)
    # advance the query x frontier running matrix by one hop. effective_
    # connectivity aligns step's rows to colnames(running) by name (dead-end
    # paths -> zero). normalise=FALSE: running is already a product of
    # normalised matrices and step was normalised above, so neither is rescaled.
    step <- if (partners == "outputs") Mn else Matrix::t(Mn)
    running <- if (is.null(running)) step
               else coconat::effective_connectivity(list(running, step),
                                                     normalise = FALSE)

    # group label of each new-frontier neuron (partner side of this hop).
    # group=FALSE means each neuron is its own group, so pruning becomes a
    # per-neuron rather than per-type cut and the terminal layer stays ungrouped.
    ntypes <- if (isFALSE(group)) {
      stats::setNames(colnames(running), colnames(running))
    } else {
      nt <- tbl[[group]][!duplicated(tbl[[newfrontier_key]])]
      names(nt) <- tbl[[newfrontier_key]][!duplicated(tbl[[newfrontier_key]])]
      nt[colnames(running)]
    }

    if (h <= nhops) {
      # intermediate layer: prune frontier by group (selection only). With
      # group=FALSE the grouping is the identity so we skip the multiplication.
      grp <- if (isFALSE(group)) running
             else running %*% coconat::grouping_matrix(colnames(running), ntypes)
      keep_types <- colnames(grp)[apply(as.matrix(grp), 2, max) >= mf[h]]
      surviving <- colnames(running)[ntypes %in% keep_types]
      if (remove_query)
        surviving <- setdiff(surviving, query_keys)
      running <- running[, surviving, drop = FALSE]
      frontier <- surviving
    } else {
      far_types <- ntypes
    }
  }

  # terminal: optionally drop query neurons from the target set (a query type
  # can still appear as a feature if non-query neurons of that type are targets)
  if (remove_query) {
    keepcols <- setdiff(colnames(running), query_keys)
    running <- running[, keepcols, drop = FALSE]
    far_types <- far_types[keepcols]
  }
  # group far neurons to type (per-neuron normalisation already done). With
  # group=FALSE the terminal layer is left at neuron resolution.
  eff <- if (isFALSE(group)) running
         else running %*% coconat::grouping_matrix(colnames(running), far_types)
  # final cut
  eff <- Matrix::drop0(eff * (eff >= mf[nhops + 1L]))
  eff
}

# Melt an effective query x target matrix into a cf_partners-like long table for
# one direction, with the query on the appropriate key column and the target in
# `group`. Feeds straight into multi_cosine_matrix. When group=FALSE the target
# is a neuron key, which multi_cosine_matrix reads from post_key/pre_key
# directly, so no grouping column is added.
.mh_matrix2df <- function(eff, dataset, partners, group = "type") {
  if (is.null(eff) || length(eff) == 0 || sum(eff != 0) == 0)
    return(NULL)
  s <- Matrix::summary(Matrix::drop0(eff))
  qk <- rownames(eff)[s$i]
  tp <- colnames(eff)[s$j]
  df <- data.frame(stringsAsFactors = FALSE, weight = s$x)
  if (!isFALSE(group))
    df[[group]] <- tp
  # query lives in pre_key for outputs, post_key for inputs
  if (partners == "outputs") {
    df$pre_key <- qk
    df$post_key <- tp
  } else {
    df$post_key <- qk
    df$pre_key <- tp
  }
  # pre_id/post_id kept so the table is recognised by is.mct(); they are not
  # coerced downstream (multi_cosine_matrix uses standardise_input=FALSE)
  df$pre_id <- sub("^[a-z]+:", "", df$pre_key)
  df$post_id <- df$post_key
  df$dataset <- dataset
  df$partners <- partners
  df
}

# Effective connectivity long table for ONE direction across all datasets.
# Mirrors the per-direction output of cf_partners (before match_types) so it can
# be dropped into multi_connection_table.
multihop_partner_summary <- function(kk, partners, nhops, threshold, min_frac,
                                     group = "type", remove_query = FALSE,
                                     MoreArgs = list()) {
  kk <- keys(kk)
  kdf <- keys2df(kk)
  datasets <- unique(kdf$dataset)
  res <- lapply(datasets, function(ds) {
    dskeys <- kk[kdf$dataset == ds]
    ma <- if (!is.null(MoreArgs[[ds]])) MoreArgs[ds] else list()
    eff <- multihop_effective_matrix(
      dskeys, partners = partners, nhops = nhops, threshold = threshold,
      min_frac = min_frac, group = group, remove_query = remove_query,
      MoreArgs = ma)
    .mh_matrix2df(eff, dataset = ds, partners = partners, group = group)
  })
  dplyr::bind_rows(res)
}

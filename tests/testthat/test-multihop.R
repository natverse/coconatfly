# Offline tests of the multihop effective-connectivity core. cf_partners is
# mocked with a small synthetic 3-layer network so no live service is needed.
#
# Network (outputs / downstream):
#   query      q1 (fw:1), q2 (fw:2)
#   layer1     i11 (fw:11, type A), i12 (fw:12, type B)
#   layer2     t21 (fw:21, type X), t22 (fw:22, type Y)

synthetic_edges <- function() {
  data.frame(
    pre_key  = c("fw:1","fw:2","fw:2", "fw:11","fw:11","fw:12"),
    post_key = c("fw:11","fw:11","fw:12", "fw:21","fw:22","fw:22"),
    weight   = c(10, 5, 5, 8, 2, 4),
    stringsAsFactors = FALSE)
}

# type of each neuron (partner side)
synthetic_type <- function(key) {
  lu <- c("fw:11"="A","fw:12"="B","fw:21"="X","fw:22"="Y",
          "fw:1"="Q","fw:2"="Q")
  unname(lu[key])
}

mock_cf_partners <- function(ids, threshold=1L, partners="outputs",
                             MoreArgs=list(), ...) {
  e <- synthetic_edges()
  keycol <- if(partners=="outputs") "pre_key" else "post_key"
  partcol <- if(partners=="outputs") "post_key" else "pre_key"
  e <- e[e[[keycol]] %in% as.character(ids) & e$weight >= threshold, , drop=FALSE]
  if(nrow(e)==0) return(NULL)
  e$type <- synthetic_type(e[[partcol]])
  e$dataset <- "flywire"
  e
}

test_that("multihop_effective_matrix matches hand-computed one-hop", {
  testthat::local_mocked_bindings(cf_partners = mock_cf_partners)
  eff <- coconatfly:::multihop_effective_matrix(
    c("fw:1","fw:2"), partners="outputs", nhops=1L,
    threshold=1L, min_frac=0, group="type")
  m <- as.matrix(eff)
  # expected (see header): q1 X=2/3 Y=2/9 ; q2 X=1/3 Y=7/9
  expect_setequal(rownames(m), c("fw:1","fw:2"))
  expect_setequal(colnames(m), c("X","Y"))
  m <- m[c("fw:1","fw:2"), c("X","Y")]
  expect_equal(unname(m),
    matrix(c(2/3, 2/9, 1/3, 7/9), nrow=2, byrow=TRUE))
})

test_that("frontier pruning by min_frac drops weak interneuron types", {
  testthat::local_mocked_bindings(cf_partners = mock_cf_partners)
  # interneuron type A max effective input-fraction = 2/3, type B = 1.
  # min_frac 0.7 prunes A, keeping only i12 (type B) -> only target Y via q2.
  eff <- coconatfly:::multihop_effective_matrix(
    c("fw:1","fw:2"), partners="outputs", nhops=1L,
    threshold=1L, min_frac=0.7, group="type")
  m <- as.matrix(eff)
  expect_equal(colnames(m), "Y")
  # q2 -> i12 -> t22 fully: 1 ; q1 has no path through i12
  expect_equal(m["fw:2","Y"], 1)
  expect_true(!"fw:1" %in% rownames(m) || m["fw:1","Y"]==0)
})

test_that("remove_query drops query neurons from the target set", {
  # add a recurrent path i11 -> q1 (fw:1, type Q) so a query neuron is a target
  edges2 <- rbind(synthetic_edges(),
                  data.frame(pre_key="fw:11", post_key="fw:1", weight=6,
                             stringsAsFactors=FALSE))
  typ2 <- c("fw:11"="A","fw:12"="B","fw:21"="X","fw:22"="Y","fw:1"="Q","fw:2"="Q")
  mock2 <- function(ids, threshold=1L, partners="outputs", MoreArgs=list(), ...) {
    kc <- if(partners=="outputs")"pre_key" else "post_key"
    pc <- if(partners=="outputs")"post_key" else "pre_key"
    e <- edges2[edges2[[kc]] %in% as.character(ids) & edges2$weight>=threshold,,drop=FALSE]
    if(nrow(e)==0) return(NULL)
    e$type <- unname(typ2[e[[pc]]]); e$dataset <- "flywire"; e
  }
  testthat::local_mocked_bindings(cf_partners = mock2)
  # without removal, the query neuron fw:1 (type Q) shows up as a feature
  eff0 <- coconatfly:::multihop_effective_matrix(
    c("fw:1","fw:2"), "outputs", 1L, 1L, 0, "type", remove_query=FALSE)
  expect_true("Q" %in% colnames(eff0))
  # with removal it is dropped (no other Q target exists)
  eff1 <- coconatfly:::multihop_effective_matrix(
    c("fw:1","fw:2"), "outputs", 1L, 1L, 0, "type", remove_query=TRUE)
  expect_false("Q" %in% colnames(eff1))
})

test_that("untyped intermediate neurons still propagate connectivity", {
  # q1 -> i1(type A), i2(UNTYPED) ; i1 -> t1(X) ; i2 -> t2(Y). The whole point
  # of multihop is to reach a well typed layer through a poorly typed one, so
  # the untyped interneuron must not silently discard the path to Y.
  edges <- data.frame(
    pre_key  = c("fw:1","fw:1","fw:11","fw:12"),
    post_key = c("fw:11","fw:12","fw:21","fw:22"),
    weight   = c(10,10,8,8), stringsAsFactors=FALSE)
  mk <- function(typ) function(ids, threshold=1L, partners="outputs",
                               MoreArgs=list(), ...) {
    e <- edges[edges$pre_key %in% as.character(ids) & edges$weight>=threshold,,drop=FALSE]
    if(nrow(e)==0) return(NULL)
    e$type <- unname(typ[e$post_key]); e$dataset <- "flywire"; e
  }

  testthat::local_mocked_bindings(
    cf_partners = mk(c("fw:11"="A","fw:12"=NA,"fw:21"="X","fw:22"="Y")))
  eff <- coconatfly:::multihop_effective_matrix(
    "fw:1", "outputs", 1L, 1L, 0, "type")
  expect_setequal(colnames(eff), c("X","Y"))
})

test_that("untyped terminal partners are dropped (as at nhops=0)", {
  edges <- data.frame(
    pre_key  = c("fw:1","fw:1","fw:11","fw:12"),
    post_key = c("fw:11","fw:12","fw:21","fw:22"),
    weight   = c(10,10,8,8), stringsAsFactors=FALSE)
  typ <- c("fw:11"="A","fw:12"="B","fw:21"="X","fw:22"=NA)
  testthat::local_mocked_bindings(
    cf_partners = function(ids, threshold=1L, partners="outputs",
                           MoreArgs=list(), ...) {
      e <- edges[edges$pre_key %in% as.character(ids),,drop=FALSE]
      if(nrow(e)==0) return(NULL)
      e$type <- unname(typ[e$post_key]); e$dataset <- "flywire"; e
    })
  eff <- coconatfly:::multihop_effective_matrix(
    "fw:1", "outputs", 1L, 1L, 0, "type")
  # an untyped target cannot serve as a shared feature, so only X remains
  expect_equal(colnames(eff), "X")
})

test_that("group=FALSE keeps targets at neuron resolution", {
  testthat::local_mocked_bindings(cf_partners = mock_cf_partners)
  # same network/values as the hand-computed one-hop test, but ungrouped: the
  # single X neuron is fw:21 and the single Y neuron is fw:22
  eff <- coconatfly:::multihop_effective_matrix(
    c("fw:1","fw:2"), partners="outputs", nhops=1L,
    threshold=1L, min_frac=0, group=FALSE)
  m <- as.matrix(eff)
  expect_setequal(colnames(m), c("fw:21","fw:22"))
  m <- m[c("fw:1","fw:2"), c("fw:21","fw:22")]
  expect_equal(unname(m), matrix(c(2/3, 2/9, 1/3, 7/9), nrow=2, byrow=TRUE))
})

test_that("group=FALSE prunes the frontier per neuron", {
  testthat::local_mocked_bindings(cf_partners = mock_cf_partners)
  # per-neuron effective input fractions to the interneurons: i11 max 2/3,
  # i12 max 1. min_frac 0.7 prunes i11, leaving only q2 -> i12 -> fw:22
  eff <- coconatfly:::multihop_effective_matrix(
    c("fw:1","fw:2"), partners="outputs", nhops=1L,
    threshold=1L, min_frac=0.7, group=FALSE)
  m <- as.matrix(eff)
  expect_equal(colnames(m), "fw:22")
  expect_equal(unname(m["fw:2","fw:22"]), 1)
})

test_that(".mh_matrix2df omits the grouping column when group=FALSE", {
  m <- Matrix::Matrix(c(2/3, 1/3), nrow=1,
    dimnames=list("fw:1", c("fw:21","fw:22")))
  df <- coconatfly:::.mh_matrix2df(m, dataset="flywire", partners="outputs",
                                   group=FALSE)
  expect_false("FALSE" %in% colnames(df))
  expect_false("type" %in% colnames(df))
  # multi_cosine_matrix reads the ungrouped target straight from post_key
  expect_setequal(df$post_key, c("fw:21","fw:22"))
  expect_equal(unique(df$pre_key), "fw:1")
})

test_that("nhops=2 handles an interneuron dead end (row alignment)", {
  # q1 -> {i1(A), i2(B)} ; i1 -> j1(X) ; i2 has NO outputs (dead end) ;
  # j1 -> t1(Z). With alignment, i2's missing onward row must become zero.
  edges3 <- data.frame(
    pre_key  = c("fw:1","fw:1", "fw:11", "fw:21"),
    post_key = c("fw:11","fw:12","fw:21", "fw:31"),
    weight   = c(10, 5, 8, 6), stringsAsFactors=FALSE)
  typ3 <- c("fw:11"="A","fw:12"="B","fw:21"="X","fw:31"="Z")
  mock3 <- function(ids, threshold=1L, partners="outputs", MoreArgs=list(), ...) {
    e <- edges3[edges3$pre_key %in% as.character(ids) & edges3$weight>=threshold,,drop=FALSE]
    if(nrow(e)==0) return(NULL)
    e$type <- unname(typ3[e$post_key]); e$dataset <- "flywire"; e
  }
  testthat::local_mocked_bindings(cf_partners = mock3)
  eff <- coconatfly:::multihop_effective_matrix(
    "fw:1", "outputs", nhops=2L, threshold=1L, min_frac=0, group="type")
  m <- as.matrix(eff)
  # only surviving path q1 -> i1 -> j1 -> t1(Z); each step fully normalised -> 1
  expect_equal(colnames(m), "Z")
  expect_equal(unname(m["fw:1","Z"]), 1)
})

test_that(".mh_matrix2df melts to a cf_partners-like table", {
  m <- Matrix::Matrix(c(2/3, 2/9, 1/3, 7/9), nrow=2, byrow=TRUE,
    dimnames=list(c("fw:1","fw:2"), c("X","Y")))
  df <- coconatfly:::.mh_matrix2df(m, dataset="flywire", partners="outputs")
  expect_true(all(c("pre_key","post_key","type","weight","dataset","partners",
                    "pre_id","post_id") %in% colnames(df)))
  expect_setequal(df$pre_key, c("fw:1","fw:2"))
  expect_setequal(df$type, c("X","Y"))
  expect_equal(nrow(df), 4L)
  expect_true(is.mct(transform(df)) || all(c("pre_id","post_id") %in% colnames(df)))
})

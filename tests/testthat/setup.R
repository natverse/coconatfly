# Use 5+ digit IDs to satisfy is_key() validation
.rhubarb_ids_vec <- c(10001L, 10002L, 10003L)

.rhubarb_meta <- function(ids=NULL) {
  df=data.frame(
    id = .rhubarb_ids_vec,
    side='R',
    type = c("G1_PN", "G1.2_PN", "G2_PN"),
    group = c(10001, 10002, 10004),
    instance = c("G1_PN_R", "G1.2_PN_R", "G2_PN_R"),
    class = c("central", "central", "central"),
    subclass = c("ALPN", "ALPN", "ALPN"),
    subsubclass = c("bilateral mALT PN", "bilateral mALT PN", "mALT PN"),
    lineage = c(NA, NA, "lPN"))
  if(is.null(ids)) {
    df
  } else if(is.numeric(ids) || is.character(ids)) {
    # Handle both numeric and character IDs
    ids_num <- as.numeric(ids)
    df[df$id %in% ids_num, , drop=FALSE]
  } else if(length(ids)==1 && grepl("type:", ids)) {
    query=sub("type:", "", ids)
    df=df[grepl(query, df$type),]
  } else {
    stop("unsupported query")
  }
}

.rhubarb_ids <- function(ids) {
  # Simple id function for test dataset - just returns numeric ids
  if(is.numeric(ids)) return(ids)
  # If query, resolve via meta function
  if(is.character(ids) && length(ids)==1 && grepl("/", ids)) {
    query <- sub("^/", "", ids)
    df <- .rhubarb_meta(query)
    return(df$id)
  }
  ids
}

# Returns minimal partner data (just ids + weight) to test metadata enrichment
.rhubarb_partners <- function(ids, partners = c("inputs", "outputs"), threshold = 1L, ...) {
  partners <- match.arg(partners)
  # Fake connectivity: each neuron connects to the others
  # Using 5+ digit IDs to satisfy is_key() validation
  edges <- data.frame(
    pre_id = c(10001, 10001, 10002, 10002, 10003, 10003),
    post_id = c(10002, 10003, 10001, 10003, 10001, 10002),
    weight = c(10, 10, 20, 20, 30, 30)
  )
  # Filter by query ids and direction
  if (partners == "outputs") {
    edges <- edges[edges$pre_id %in% ids & edges$weight >= threshold, ]
  } else {
    edges <- edges[edges$post_id %in% ids & edges$weight >= threshold, ]
  }
  edges
}

register_rhubarb <- function() {
  if(! 'rhubarb' %in% cf_datasets()) {
    coconat::register_dataset('rhubarb', shortname = 'rb',
                              species = 'Rheum rhabarbarum', sex='U', age='adult',
                              metafun=.rhubarb_meta,
                              idfun=.rhubarb_ids,
                              partnerfun=.rhubarb_partners,
                              namespace = 'coconatfly')
  }
  if(! 'badrhubarb' %in% cf_datasets()) {
    coconat::register_dataset('badrhubarb', shortname = 'bb',
                              species = 'Rheum rhabarbarum', sex='U', age='adult',
                              namespace = 'coconatfly')
  }
}

# withr::defer(rm(register_rhubarb), teardown_env())

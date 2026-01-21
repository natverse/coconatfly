normalise_query <- function(x, dataset=NULL) {
  if(!isTRUE(id_spec_type(x)=='query'))
    return(x)

  if(!isTRUE(substr(x,1,1)=="/") || length(x)!=1)
    return(x)

  # first add default query field if missing
  query_field=stringr::str_match(x, '^/([A-z][A-z0-9]*):.+')[,2]
  if(is.na(query_field)) {
    query_field='type'
    x=paste0('/type:', substr(x,2,nchar(x)))
  }
  query_field2=translate_fields(query_field, dataset = dataset, direction = 'out')
  query_expr=stringr::str_match(x, '^[^:]+:(.+)')[,2]
  if(is.na(query_expr)) {
    warning('unable to parse query expression!')
    return(x)
  }
  x=paste0("/", query_field2, ":", query_expr)
  x
}

# this private function; expects a character vector and returns one of same length
# out means from coconatfly to the external data source
# in means from the external data source to coconatfly
translate_fields <- function(x, dataset, direction=c("out", "in")) {
  FUN=field_translater(dataset=dataset, direction = direction)
  FUN(x)
}

# this is a function generator, the resultant functions are what dplyr::rename
# would like
field_translater <- function(dataset, direction=c("out", "in")) {
  dataset=match_datasets(dataset)
  direction=match.arg(direction)
  field_table <- switch(dataset,
                        flywire=c(
                          id='root_id',
                          type="cell_type",
                          class="super_class",
                          subclass="cell_class",
                          subsubclass="cell_sub_class"),
                        hemibrain=c(
                          id='bodyid',
                          lineage="cellBodyFiber"
                        ),
                        manc=c(
                          id='bodyid',
                          lineage="hemilineage"
                        ),
                        malecns=c(
                          id='bodyid',
                          class="superclass",
                          subclass="class",
                          subsubclass="subclass"
                        ),
                        opticlobe=c(
                          id='bodyid'
                        ),
                        character()
  )

  FUN=function(x) {
    if(length(field_table)==0) return(x)
    # out means that we are translating coconatfly -> external
    # in means that we are translating external -> coconatfly
    if(direction=='out') {
      y=field_table[match(x, names(field_table))]
      y[is.na(y)]=x[is.na(y)]
    } else {
      y=names(field_table)[match(x, field_table)]
      y[is.na(y)]=x[is.na(y)]
    }
    unname(y)
  }
  FUN
}


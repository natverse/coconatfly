register_rhubarb <- function() {
  if(! 'rhubarb' %in% cf_datasets()) {
    coconat::register_dataset('rhubarb', shortname = 'rb',
                              species = 'Rheum rhabarbarum', sex='U', age='adult',
                              namespace = 'coconatfly')
  }
}

# withr::defer(rm(register_rhubarb), teardown_env())

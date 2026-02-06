.onLoad <- function(libname, pkgname) {
  register_builtin_datasets()
}

register_builtin_datasets <- function() {
  coconat::register_dataset(
    "flywire",
    metafun = .flywire_meta,
    idfun = .flywire_ids,
    partnerfun = .flywire_partners,
    shortname = "fw",
    namespace = "coconatfly"
  )

  coconat::register_dataset(
    "hemibrain",
    metafun = .hemibrain_meta,
    idfun = .hemibrain_ids,
    partnerfun = .hemibrain_partners,
    shortname = "hb",
    namespace = "coconatfly"
  )

  coconat::register_dataset(
    "opticlobe",
    metafun = .opticlobe_meta,
    idfun = .opticlobe_ids,
    partnerfun = .opticlobe_partners,
    shortname = "ol",
    namespace = "coconatfly"
  )

  coconat::register_dataset(
    "malecns",
    metafun = .malecns_meta,
    idfun = .malecns_ids,
    partnerfun = .malecns_partners,
    shortname = "mc",
    namespace = "coconatfly"
  )

  coconat::register_dataset(
    "manc",
    metafun = .manc_meta,
    idfun = .manc_ids,
    partnerfun = .manc_partners,
    shortname = "mv",
    namespace = "coconatfly"
  )

  coconat::register_dataset(
    "fanc",
    metafun = .fanc_meta,
    idfun = .fanc_ids,
    partnerfun = .fanc_partners,
    shortname = "fv",
    namespace = "coconatfly"
  )

  coconat::register_dataset(
    "banc",
    metafun = .banc_meta,
    idfun = .banc_ids,
    partnerfun = .banc_partners,
    shortname = "bc",
    namespace = "coconatfly"
  )

  coconat::register_dataset(
    "yakubavnc",
    metafun = .yakubavnc_meta,
    idfun = .yakubavnc_ids,
    partnerfun = .yakubavnc_partners,
    shortname = "yv",
    namespace = "coconatfly"
  )
}

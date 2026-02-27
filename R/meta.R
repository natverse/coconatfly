npconn <- function(dataset) {
  dataset=match_datasets(dataset)
  if(dataset=='hemibrain')
    return(neuprintr::neuprint_login(
      server="https://neuprint.janelia.org",
      dataset='hemibrain:v1.2.1'))
  else if(dataset=='opticlobe')
    return(neuprintr::neuprint_login(
      server="https://neuprint.janelia.org",
      dataset='optic-lobe:v1.0.1'))
  else if(dataset=='malecns')
    return(malecns::mcns_neuprint())
  else if(dataset=='manc') {
    # we have a little problem here. If someone has chosen e.g. yakuba
    # then we need to switch back to MANC
    mds=getOption("malevnc.dataset", default = 'MANC')
    if(!mds %in% c("MANC", "VNC") )
      mds='MANC'
    withr::with_options(malevnc::choose_malevnc_dataset(mds, set = F),
      return(malevnc::manc_neuprint()))
  }
  else if(dataset=='yakubavnc')
    return(malevnc::manc_neuprint(
      dataset='yakuba-vnc',
      server = 'https://neuprint-yakuba.janelia.org'))
  else stop("neuprint connection unavailable for dataset: ", dataset)
}

get_meta_fun <- function(dataset) {
  dataset=match_datasets(dataset)
  dsd=coconat:::dataset_details(dataset, namespace = 'coconatfly')
  FUN=dsd[['metafun']]
  if(is.null(FUN))
    stop("No metadata function registered for dataset: ", dataset)
  FUN
}

#' Rename class columns to superclass hierarchy
#'
#' Renames class/subclass/subsubclass to superclass/class/subclass.
#' Only renames columns that exist in the dataframe.
#' @param df A data frame
#' @return The data frame with renamed columns
#' @noRd
rename_to_superclass <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)

  cols <- colnames(df)

  # Build the rename mapping based on existing columns
  # Must apply in reverse order: subsubclass -> subclass -> class -> superclass
  if ("subsubclass" %in% cols) {
    colnames(df)[colnames(df) == "subsubclass"] <- "subclass"
  }
  # Check original cols for subclass (before any renaming happened in this call)
  if ("subclass" %in% cols) {
    # Find position using original column names
    idx <- which(cols == "subclass")
    colnames(df)[idx] <- "class"
  }
  if ("class" %in% cols) {
    idx <- which(cols == "class")
    colnames(df)[idx] <- "superclass"
  }

  df
}

#' Harmonise top-level class values across datasets
#'
#' Uses regex-based rules to normalise class labels to malecns-style values.
#' Rules are applied in order; first match wins. For base classes (sensory,
#' motor, etc.), a domain prefix (cb/vnc/ol) is added based on dataset.
#'
#' @param class Character vector of class values
#' @param dataset Character vector of dataset names (recycled if length 1)
#' @param unknown_as_na If TRUE, return NA for unmatched values
#' @return Character vector of normalised class values
#' @noRd
harmonise_top_class_values <- function(class, dataset, unknown_as_na = FALSE) {
  if (length(class) == 0) return(class)
  class <- as.character(class)
  dataset <- as.character(dataset)
  if (length(dataset) == 1L) dataset <- rep(dataset, length(class))
  stopifnot(length(dataset) == length(class))

  # Normalise to lowercase with underscores
  toks <- tolower(trimws(class))
  toks <- gsub("[[:space:]-]+", "_", toks)
  toks <- gsub("_+", "_", toks)  # collapse multiple underscores
  toks <- gsub("^_|_$", "", toks)  # remove leading/trailing underscores

  # Helper to extract direction (ascending/descending) from token
  get_direction <- function(tok) {
    if (grepl("ascending", tok)) "ascending"
    else if (grepl("descending", tok)) "descending"
    else NA_character_
  }

  mapped <- class
  for (i in seq_along(toks)) {
    ds <- dataset[i]
    # Skip datasets already in target schema or not yet supported
    if (ds %in% c("malecns", "banc")) next

    tok <- toks[i]
    dir <- get_direction(tok)

    # Apply rules in order (first match wins)
    result <- if (grepl("sensory|sensory_tbd", tok) && !is.na(dir)) {
      paste0("sensory_", dir)
    } else if (grepl("efferent", tok) && !is.na(dir)) {
      paste0("efferent_", dir)
    } else if (grepl("^(ascending|descending)(_neuron)?$", tok) && !is.na(dir)) {
      paste0(dir, "_neuron")
    } else if (grepl("centrifugal", tok)) {
      "visual_centrifugal"
    } else if (grepl("visual.*projection|^projection$", tok)) {
      "visual_projection"
    } else if (grepl("endocrine", tok)) {
      "endocrine"
    } else if (grepl("^optic$", tok)) {
      "ol_intrinsic"
    } else if (grepl("sensory|sensory_tbd", tok)) {
      paste0(dataset_domain(ds), "_sensory")
    } else if (grepl("efferent", tok)) {
      paste0(dataset_domain(ds), "_efferent")
    } else if (grepl("motor", tok)) {
      paste0(dataset_domain(ds), "_motor")
    } else if (grepl("intrinsic|central", tok)) {
      paste0(dataset_domain(ds), "_intrinsic")
    } else {
      if (unknown_as_na) NA_character_ else class[i]
    }

    mapped[i] <- result
  }
  mapped
}

#' Get tissue domain prefix for a dataset (used for class harmonisation)
#' @noRd
dataset_domain <- function(ds) {
  switch(ds,
    manc = "vnc",
    fanc = "vnc",
    yakubavnc = "vnc",
    flywire = "cb",
    hemibrain = "cb",
    opticlobe = "ol",
    NA_character_
  )
}

#' Get tissue type for a dataset
#' @noRd
dataset_tissue <- function(ds) {
  switch(ds,
    flywire = "brain",
    hemibrain = "brain",
    opticlobe = "brain",
    manc = "vnc",
    fanc = "vnc",
    yakubavnc = "vnc",
    malecns = "cns",
    banc = "cns",
    NA_character_
  )
}

#' Get sex for a dataset from registration
#' @noRd
dataset_sex <- function(ds) {

  dsd <- coconat:::dataset_details(ds, namespace = 'coconatfly')
  sex <- dsd[['sex']]
  if (is.null(sex)) NA_character_ else sex
}

#' Normalise side values to L/R/M/NA
#'
#' Accepts various input formats (left, right, midline, L, R, M, etc.)
#' and normalises to single uppercase letters or NA.
#' @param x Character vector of side values
#' @return Character vector with values L, R, M, or NA only
#' @noRd
normalise_side <- function(x) {
  x <- as.character(x)
  x <- tolower(trimws(x))

  result <- rep(NA_character_, length(x))
  result[grepl("^l(eft)?$", x)] <- "L"
  result[grepl("^r(ight)?$", x)] <- "R"
  result[grepl("^m(idline)?$|^c(enter|entre)?$", x)] <- "M"

  result
}

#' Fetch metadata for neurons from connectome datasets
#'
#' @details \code{MoreArgs} is structured as a list with a top layer naming datasets
#'   (using the same long names as \code{\link{cf_datasets}}. The second (lower)
#'   layer names the arguments that will be passed to dataset-specific functions.
#'
#' @param ids A list of ids named by the relevant datasets (see examples) or any
#'   other input that can be processed by the \code{\link{keys}} function
#'   (including a \code{hclust} dendrogram object.)
#' @param integer64 Whether ids should be character vectors (default) or 64 bit
#'   ints (more compact but a little fragile as they rely on the \code{bit64}
#'   extension package.)
#' @param MoreArgs A named list of arguments to be passed when fetching metadata
#'   for a given function. See details.
#' @param use_superclass If \code{TRUE}, rename class/subclass/subsubclass
#'   columns to superclass/class/subclass. Can also be set via the
#'   \code{coconatfly.use_superclass} option.
#' @param bind.rows Whether to bind data.frames for each dataset together,
#'   keeping only the common columns (default \code{TRUE} for convenience but
#'   note that some columns will be dropped by unless \code{keep.all=TRUE}).
#' @param keep.all Whether to keep all columns when processing multiple datasets
#'   rather than just those in common (default=\code{FALSE} only keeps shared
#'   columns).
#'
#' @importFrom dplyr mutate rename rename_with select case_when any_of
#' @importFrom fafbseg flywire_ids
#' @export
#' @seealso \code{\link[neuprintr]{neuprint_ids}}
#' @examples
#' \donttest{
#' da2meta=cf_meta(cf_ids(hemibrain='DA2_lPN'))
#' da2meta
#' # / introduces a regular expression
#' mbonmeta=cf_meta(cf_ids(hemibrain='/MBON.+'))
#' }
cf_meta <- function(ids, bind.rows=TRUE, integer64=FALSE, keep.all=FALSE,
                    use_superclass=getOption("coconatfly.use_superclass", FALSE),
                    MoreArgs=list(flywire=list(type=c("cell_type","hemibrain_type")))) {
  if(is.character(ids) || inherits(ids, 'dendrogram') || inherits(ids, 'hclust'))
    ids=keys2df(ids)
  if(is.data.frame(ids)) {
    stopifnot(bind.rows)
    ss=split(ids$id, ids$dataset)
    res=cf_meta(ss, integer64 = integer64, MoreArgs = MoreArgs, keep.all=keep.all,
                use_superclass=use_superclass)
    res=res[match(keys(ids), res$key),,drop=F]
    return(res)
  }
  ids <- checkmate::assert_named(ids, type = 'unique')
  names(ids)=match_datasets(names(ids))
  stopifnot(all(names(ids) %in% cf_datasets('all')))

  # Check that IDs have been expanded (queries resolved)
  check_expanded(ids)

  res=vector(mode = 'list', length = length(ids))
  names(res)=names(ids)

  for(n in names(ids)) {

    args=list(ids=ids[[n]])
    args2=MoreArgs[[n]]
    if(length(args2)) args=c(args, args2)
    FUN=get_meta_fun(n)
    tres=try(do.call(FUN, args), silent = F)
    # maybe our query didn't yield anything
    if(inherits(tres, 'try-error') || is.null(tres) || !isTRUE(nrow(tres)>0))
      next
    tres$id=flywire_ids(tres$id, integer64=integer64, na_ok=TRUE)
    cols_we_want=c("id", "class", "subclass", "type", 'side', 'sex', 'tissue', 'group', "instance")
    missing_cols=setdiff(cols_we_want, colnames(tres))
    if('class' %in% missing_cols)
      tres$class=NA_character_
    if('subclass' %in% missing_cols)
      tres$subclass=NA_character_
    if('type' %in% missing_cols)
      tres$type=NA_character_
    if('group' %in% missing_cols)
      tres$group=bit64::as.integer64(NA)
    if('tissue' %in% missing_cols)
      tres$tissue=dataset_tissue(n)
    if('sex' %in% missing_cols)
      tres$sex=dataset_sex(n)
    if('side' %in% missing_cols)
      tres$side=NA_character_
    if('instance' %in% missing_cols) {
      tres <-if('name' %in% colnames(tres))
        tres %>% rename(instance=name)
      else
        tres %>%
        mutate(instance=case_when(
          !is.na(type) & !is.na(side) ~ paste0(type, "_", side),
          !is.na(type) ~ paste0(type, "_"),
          T ~ NA_character_))
    }
    tres <- tres |>
      mutate(instance=case_when(
        is.na(instance) & !is.na(type) & !is.na(side) ~ paste0(type, "_", side),
        is.na(instance) & !is.na(type) ~ paste0(type, "_"),
        !is.na(instance) ~ instance
      ))
    tres$group=flywire_ids(tres$group, integer64 = integer64)
    missing_cols=setdiff(cols_we_want, colnames(tres))
    if(length(missing_cols)>0)
      stop("We are missing columns: ", paste(missing_cols, collapse = ','))
    tres$class=harmonise_top_class_values(tres$class, n)
    if("side" %in% colnames(tres))
      tres$side=normalise_side(tres$side)
    tres$dataset=n
    tres$key=keys(tres)
    res[[n]]=tres
  }
  if(length(res)==0) return(NULL)
  res <- if(bind.rows) bind_rows2(res, keep.all=keep.all) else res
  if (isTRUE(use_superclass)) {
    res <- rename_to_superclass(res)
  }
  res
}

# Shared helper for fanc and banc metadata
fancorbanc_meta <- function(table, ids=NULL, ...) {
  ol_classes=c("centrifugal", "distal medulla", "distal medulla dorsal rim area",
               "lamina intrinsic", "lamina monopolar", "lamina tangential",
               "lamina wide field", "lobula intrinsic", "lobula lobula plate tangential",
               "lobula medulla amacrine", "lobula medulla tangential",
               "lobula plate intrinsic", "medulla intrinsic",
               "medulla lobula lobula plate amacrine", "medulla lobula tangential",
               "photoreceptors", "proximal distal medulla tangential",
               "proximal medulla", "serpentine medulla", "T neuron",
               "translobula plate", "transmedullary", "transmedullary Y",
               "Y neuron")
  fid=list(tag2=c('primary class',"anterior-posterior projection pattern",
                  "neuron identity", "soma side", ol_classes))
  fid=list(fid)
  names(fid)=table
  selc=list(c("id", "tag", "tag2", "pt_root_id", 'pt_supervoxel_id'))
  names(selc)=table

  cell_infos=fafbseg::flywire_cave_query(table, filter_in_dict=fid, select_columns=selc,
                                version='latest', timetravel = T, allow_missing_lookups=T)
  metadf <- if(nrow(cell_infos)<1) {
    df=data.frame(id=character(), class=character(), type=character(), side=character())
  } else {
    cell_infos2 <- cell_infos %>%
      mutate(
        tag=sub("\n\n\n*banc-bot*","", fixed = T, tag),
        pt_root_id=as.character(pt_root_id))
    cell_infos3 <- cell_infos2 %>%
      mutate(
        tag2=case_when(
          tag2 %in% ol_classes ~ 'neuron identity',
          T ~ tag2)
      ) %>%
      arrange(pt_root_id, tag) %>%
      distinct(pt_root_id, tag2, tag, .keep_all = T) %>%
      group_by(pt_root_id, tag2) %>%
      # summarise(tag=paste0(tag, collapse=";"), .groups = 'drop')
      summarise(tag={
        if(length(tag)>1 && any(grepl("?", tag, fixed = T))) {
          # we would like to remove duplicate tags
          # that would otherwise give: DNg75;DNg75?
          usx=unique(sub("?", "", tag, fixed = T))
          if(length(usx)<length(tag))
            tag=usx
        }
        paste0(tag, collapse=";")
      }, .groups = 'drop')

    cell_infos2.ol=cell_infos2 %>% filter(tag2 %in% ol_classes)

    cell_infos4 <-   cell_infos3 %>%
      tidyr::pivot_wider(id_cols = pt_root_id,
                         names_from = tag2,
                         values_from = tag,
                         values_fill = ""
      ) %>%
      rename(id=pt_root_id, class=`primary class`, apc=`anterior-posterior projection pattern`,
             type=`neuron identity`, side=`soma side`) %>%
      mutate(class=case_when(
        id %in% cell_infos2.ol$pt_root_id ~ "optic",
        class=='sensory neuron' & grepl('scending', apc) ~ paste('sensory', apc),
        (class=="" | class=='central neuron') & apc=='ascending' ~ 'ascending',
        (class=="" | class=='central neuron') & apc=='descending' ~ 'descending',
        apc=="" & class=="" ~ '',
        apc=="" ~ class,
        T ~ paste(class, apc)
      )) %>%
      mutate(class=sub(" neuron", '', class)) %>%
      mutate(side=sub('soma on ', '', side)) %>%
      mutate(side=case_when(
        is.na(side) ~ side,
        T ~ toupper(substr(side,1,1))
      )) %>%
      select(id, class, type, side) %>%
      mutate(subclass=NA_character_) %>%
      mutate(id=as.character(id))
  }
  if(!is.null(ids))
    dplyr::left_join(data.frame(id=ids), metadf, by='id')
  else
    metadf
}

# Shared helper for fanc and banc ID resolution
#' @importFrom dplyr pull
fancorbanc_ids <- function(ids, dataset=c("banc", "fanc")) {
  if(is.null(ids)) return(NULL)
  dataset=match.arg(dataset)
  # extract numeric ids if possible
  ids <- extract_ids(ids)
  if(is.character(ids) && length(ids)==1 && !fafbseg:::valid_id(ids)) {
    # query
    metadf=if(dataset=="banc") .banc_meta() else .fanc_meta()
    if(isTRUE(ids=='all')) return(fancr::fanc_ids(metadf$id, integer64 = F))
    if(isTRUE(ids=='neurons')) {
      ids <- metadf %>%
        filter(is.na(.data$class) | .data$class!='glia') %>%
        pull(.data$id)
      return(fancr::fanc_ids(ids, integer64 = F))
    }
    if(isTRUE(substr(ids, 1, 1)=="/"))
      ids=substr(ids, 2, nchar(ids))
    else warning("All FANC/BANC queries are regex queries. ",
              "Use an initial / to suppress this warning!")
    if(!grepl(":", ids)) ids=paste0("type:", ids)
    qsplit=stringr::str_match(ids, pattern = '[/]{0,1}(.+):(.+)')
    field=qsplit[,2]
    value=qsplit[,3]
    if(!field %in% colnames(metadf)) {
      stop(glue("{dataset} queries only work with these fields: ",
           paste(colnames(metadf)[-1], collapse = ',')))
    }
    ids <- metadf %>%
      filter(grepl(value, .data[[field]])) %>%
      pull(.data$id)
  } else if(length(ids)>0) {
    # check they are valid for current materialisation
    ids <- if(dataset=="banc")
      fancr::with_banc(fafbseg::flywire_latestid(ids, version = banc_version()))
    else
      fancr::with_fanc(fafbseg::flywire_latestid(ids, version = fanc_version()))
  }
  return(fancr::fanc_ids(ids, integer64 = FALSE))
}

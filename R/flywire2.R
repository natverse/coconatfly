#' Use FlyWire v2 (Princeton) synapses as an extra dataset with coconatfly
#'
#' @description Register the v2 connectivity information (Princeton synapse) for
#'   FlyWire as an additional dataset for use with coconatfly for across dataset
#'   connectome analysis.
#'
#' @details The v2 synapses still reference the same v783 segmentation and
#' metadata but
#'
#'
#' @param name The dataset name to use.
#' @param showerror Logical: When set to FALSE will error out silently instead
#'   of showing error messages.
#' @param ... Additional arguments passed to
#'   \code{coconat::\link[coconat]{register_dataset}}
#' @export
#'
#' @examples
#' \dontrun{
#' library(coconatfly)
#' # once per session
#' register_flywire2()
#'
#' # examples of within dataset analysis
#' dna02meta <- cf_meta(cf_ids(fx='/type:DNa02', flywire='/type:DNa02'))
#' dna02.comp=cf_partner_summary(dna02meta, partners = 'out', threshold = 10)
#' cf_ids(banc='/type:DNa.+')
#'
#' # an example of across dataset cosine similarity plot
#' cf_cosine_plot(cf_ids('/type:LAL0(08|09|10|42)',
#'   datasets = c("flywire", "fx")))
#' }
register_flywire2 <- function(name='fx', ..., showerror=TRUE){
  coconat::register_dataset(
    name = name,
    namespace = 'coconatfly',
    partnerfun = function(ids, version=NULL, ...) {
      .flywire_partners(ids, version='783.2', ...)
    },
    metafun=function(ids, ...) {
      coconatfly:::flywire_meta(ids, ...)
    },
    ...
  )
}

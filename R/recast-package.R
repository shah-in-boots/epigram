#' recast package
#'
#' @docType package
#' @aliases recast-package
#' @keywords internal
"_PACKAGE"

#' Internal vctrs methods
#'
#' @import vctrs
#' @importFrom lifecycle deprecated
#' @importFrom methods setOldClass
#' @keywords internal
#' @name recast-vctrs
NULL

#' Internal S7 machinery
#'
#' `recast` uses S7 for its *scalar specification and build* objects (the
#' `<mdl_gt>` table spec and its resolved pipeline) while keeping its *vector*
#' types (`tm`, `fmls`, `mdl`, `mdl_tbl`) on vctrs — the two class systems are
#' complementary layers, not competitors. See `vignette("s7")`.
#'
#' @importFrom S7 new_class new_object new_generic new_S3_class S7_object method methods_register
#' @importFrom S7 class_data.frame class_list class_integer class_any
#' @rawNamespace importFrom(S7, "method<-")
#' @keywords internal
#' @name recast-s7
NULL


# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

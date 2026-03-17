##' Lineage-count function
##'
##' Lineage-counts, saturations, and event-codes.
##' Returns a list of per-segment data frames.
##'
##' @name lineages
##' @include getinfo.R
##' @inheritParams getInfo
##' @return A list of per-segment data frames containing lineage information.
##' @rdname lineages
##' @export
lineages <- function (object, prune = TRUE, obscure = TRUE) {
  getInfo(object,lineages=TRUE,prune=prune,obscure=obscure) |>
    getElement("lineages")
}

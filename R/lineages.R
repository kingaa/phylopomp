##' Lineage-count function
##'
##' The number of lineages through time
##'
##' @name lineages
##' @include getinfo.R
##'
##' @inheritParams getInfo
##' @return A tibble containing the lineage count function.
##' @rdname lineages
##' @export
lineages <- function (data, prune = TRUE) {
  getInfo(data,lineages=TRUE,prune=prune) |>
    getElement("lineages")
}

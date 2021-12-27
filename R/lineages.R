##' Lineage-count function
##'
##' The number of lineages through time
##'
##' @name lineages
##' @include getinfo.R
##'
##' @inheritParams getInfo
##' @return A \code{\link[tibble]{tibble}} containing the lineage count function.
##' If the genealogy has been obscured (the default), the number in the \code{lineages}
##' column is the total number of lineages present at the times in the \code{time} column.
##' If the genealogy has not been obscured (\code{obscure = FALSE}), the deme-specific
##' lineage counts are returned.
##' 
##' @example examples/lineages.R
##' 
##' @rdname lineages
##' @export
lineages <- function (object, prune = TRUE, obscure = TRUE) {
  getInfo(object,lineages=TRUE,prune=prune,obscure=obscure) |>
    getElement("lineages")
}

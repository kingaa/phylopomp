##' Newick output
##'
##' Extract a Newick-format description of a genealogy.
##' Returns a character vector with one tree per segment.
##'
##' @name newick
##' @include getinfo.R
##' @inheritParams getInfo
##' @return A character vector of Newick-format trees (one per segment).
##' @rdname newick
##' @export
newick <- function (object, prune = TRUE, obscure = TRUE,
                    hide = FALSE, compact = TRUE) {
  getInfo(object,tree=TRUE,prune=prune,obscure=obscure,
          hide=hide,compact=compact) |>
    getElement("tree")
}

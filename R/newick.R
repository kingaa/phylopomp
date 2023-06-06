##' Newick output
##'
##' Extract a Newick-format description of a genealogy.
##'
##' @name newick
##' @include getinfo.R
##' @inheritParams getInfo
##' @return A string in Newick format.
##' @examples
##' simulate("SIIR",time=1) |> newick()
##' @rdname newick
##' @export
newick <- function (object, prune = TRUE, obscure = TRUE) {
  getInfo(object,tree=TRUE,prune=prune,obscure=obscure) |>
    getElement("tree")
}

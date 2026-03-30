##' Newick output
##'
##' Extract a Newick-format description of a genealogy.
##'
##' @name newick
##' @include getinfo.R
##' @inheritParams getInfo
##' @param extended logical; if TRUE, an extended-Newick format is used.
##' In particular, metadata tags of the form `[&&PhyloPOMP:...]` are inserted.
##' These contain information regarding node-type and deme.
##'
##' If `extended = FALSE`, then the string returned is in ordinary Newick format.
##' Deme and node-type information is discarded.
##' Note that `extended = FALSE` implies both `prune = TRUE` and `obscure = TRUE`.
##' @return A string in Newick format.
##' @examples
##' simulate("SIIR",time=1) |> newick()
##' @rdname newick
##' @export
newick <- function (
  object,
  prune = TRUE,
  obscure = TRUE,
  extended = TRUE
) {
  if (!extended && !(prune && obscure))
    pStop("if `extended=FALSE`, both `prune` and `obscure` must be `TRUE`.")
  getInfo(
    object,
    newick=TRUE,
    prune=prune,
    obscure=obscure,
    extended=extended
  ) |>
    getElement("newick")
}

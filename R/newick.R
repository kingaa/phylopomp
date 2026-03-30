##' Newick output
##'
##' Extract a Newick-format description of a genealogy.
##'
##' @name newick
##' @include getinfo.R
##' @inheritParams getInfo
##' @param extended logical; if TRUE, an extended-Newick format is used.
##' See Details.
##' @details
##' In the extended-Newick format, metadata tags of the form \code{[&&PhyloPOMP ...]} are inserted.
##' These contain information regarding node-type and deme.
##' See below for more information.
##'
##' If \code{extended = FALSE}, then the string returned is in ordinary Newick format.
##' Deme and node-type information is discarded.
##' Every sample becomes a tip (inline samples lie on the ends of branches of zero length).
##' Note that \code{extended = FALSE} implies both \code{prune = TRUE} and \code{obscure = TRUE}.
##' @section Metadata tags:
##' Metadata tags are of the form
##' \preformatted{[&&PhyloPOMP type={node|sample|extant|root|migration|branch} deme=<integer>].}
##' Types \sQuote{branch}, \sQuote{node}, \sQuote{migration}, and \sQuote{root} are equivalent on input to \code{\link{parse_newick}};
##' \sQuote{type=node} is used in output of \code{\link{newick}}.
##' Note that \code{0} is reserved for the \dQuote{undeme}, i.e., unspecified or unknown deme.
##' @return A string in (possibly extended) Newick format.
##' @example examples/newick.R
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

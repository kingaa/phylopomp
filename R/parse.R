##' parse a Newick string
##'
##' Parses a Newick description and returns a binary version of the genealogy.
##'
##' \code{parse_newick} parses a string containing information in an extended Newick format and returns a \pkg{phylopomp} genealogy.
##'
##' The extension allows metadata (enclosed by square brackets).
##' It ignores metadata unless they are specifically flagged as PhyloPOMP metadata, i.e.:
##' \preformatted{
##'    [&&PhyloPOMP:...]
##' }
##' PhyloPOMP metadata can be used to encode the node type (i.e., \code{type=sample}, \code{type=extant}, \code{type=node}) and/or the deme (given as an integer, e.g., \code{deme=1}).
##' @name parse_newick
##' @param x character; the Newick description.
##' See Details for specifics.
##' @param t0 numeric; the root time.
##' @param tf numeric; the current or final time.
##' @importFrom tibble tibble
##' @return
##' An object of class \dQuote{gpgen}.
##' @rdname parse
##' @export
parse_newick <- function (x, t0 = 0, tf = NA) {
  .Call(P_parse_newick,x,t0,tf) |>
    structure(class="gpgen")
}

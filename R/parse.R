##' parse a Newick string
##'
##' Parses a Newick description and returns a binary version of the genealogy.
##'
##' @name parse_newick
##' @param x character; the Newick description.
##' See Details for specifics.
##' @param t0 numeric; the root time.
##' @param tf numeric; the current or final time.
##' @importFrom tibble tibble
##' @return
##' An object of class \dQuote{gpsim}.
##' @rdname parse
##' @export
parse_newick <- function (x, t0 = 0, tf = NA) {
  .Call(P_parse_newick,x,t0,tf) |>
    structure(class="gpsim")
}

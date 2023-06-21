##' Convert a tree in Newick format to data frame
##'
##' Convert a genealogical tree in Newick format to a data frame suitable for use with \pkg{pomp}.
##'
##' @rdname newick2df
##' @include parse.R
##' @inheritParams parse_newick
##' @return A data frame suitable for use as \code{pomp} input, containing three columns:
##' \describe{
##'   \item{time}{numeric; time of the genealogy event.}
##'   \item{lineages}{integer; the value of the lineage-count function at the specified time.
##'     Note that this function is right-continuous with left limits, and constant on the inter-event intervals.
##'   }
##'   \item{code}{integer; a code describing the nature of the event.
##'     1 indicates a coalescence;
##'     0 indicates a dead sample;
##'    -1 indicates a live sample;
##'     2 indicates a root.
##'   }
##' }
##' @importFrom tibble tibble
##' @importFrom dplyr bind_rows
##' @example examples/newick2df.R
##' @export
newick2df <- function (x, t0 = 0, tf = NA) {
  if (missing(x) || is.null(x) || !is.character(x))
    pStop(sQuote("x")," must be furnished as a string in Newick format.")
  parse_newick(x,t0,tf) |>
    getInfo(lineages=TRUE) |>
    getElement("lineages")
}

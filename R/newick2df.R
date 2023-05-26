##' Convert a tree in Newick format to data frame
##'
##' Convert a genealogical tree in Newick format to a data frame suitable for use with \pkg{pomp}.
##'
##' @rdname newick2df
##' @include parse.R
##' @param tree tree data in Newick format.
##' @param time time of the genealogy.
##' @param t0 time of the root.
##' @details
##' If \code{time} is furnished, it is assumed that the absence of samples between the latest leaf and \code{time} is informative.
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
newick2df <- function (tree, time = NA, t0 = 0) {
  if (missing(tree) || is.null(tree) || !is.character(tree))
    pStop(sQuote("tree")," must be furnished as a string in Newick format.")
  time <- as.numeric(time)
  t0 <- as.numeric(t0)
  tree |>
    parse_newick(t0=as.numeric(t0),lineages=TRUE,time=TRUE) -> x
  dat <- x$lineages
  if (length(time)>0 && !is.na(time) && time > x$time) {
    dat |>
      bind_rows(
        tibble(time=time)
      ) -> dat
  }
  dat
}

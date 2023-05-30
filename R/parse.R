##' parse_newick
##'
##' Parse a Newick description and extract various equivalent representations.
##'
##' @param x character; the Newick description.
##' See Details for specifics.
##' @param t0 numeric; the root time.
##' @inheritParams getInfo
##' @include getinfo.R
##' @importFrom tibble tibble
##' @return
##' A list containing the requested elements, including any or all of:
##' \describe{
##'   \item{time}{the current time}
##'   \item{description}{a human readable description of the state of the genealogy process}
##'   \item{yaml}{the state of the genealogy process in YAML format}
##'   \item{structure}{the state of the genealogy process in \R list format}
##'   \item{lineages}{a \code{\link[tibble]{tibble}} containing the lineage count function through time}
##'   \item{tree}{the genealogical tree, in Newick format}
##' }
##' @rdname parse
##' @export
parse_newick <- function (
  x, prune = TRUE, obscure = TRUE, t0 = 0, time = FALSE,
  description = FALSE, structure = FALSE, yaml = FALSE,
  lineages = TRUE, tree = FALSE)
{
  x <- .Call(
    P_parse_newick,
    x,t0,prune,obscure,time,
    description,yaml,structure,lineages,tree
  )
  if (!is.null(x$lineages))
    x$lineages |> reshape_lineages() -> x$lineages
  x
}

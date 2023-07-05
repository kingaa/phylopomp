##' getInfo
##'
##' Retrieve information from genealogy process simulation
##'
##' @name getInfo
##' @keywords internal
##' @param object \code{gpsim} object.
##' @param prune logical; prune the genealogy?
##' @param obscure logical; obscure the demes?
##' @param time logical; return the current time?
##' @param t0 logical; return the zero-time?
##' @param newick logical; return a Newick-format description of the tree?
##' @param description logical; return the description?
##' @param yaml logical; return the structure in YAML format?
##' @param structure logical; return the structure in \R list format?
##' @param ndeme logical; return the number of demes?
##' @param nsample logical; return the number of samples?
##' @param lineages logical; return the lineage-count function?
##' @param genealogy logical; return the lineage-traced genealogy?
##' @include package.R geneal.R
##' @importFrom tibble as_tibble
##' @return
##' A list containing the requested elements, including any or all of:
##' \describe{
##'   \item{t0}{the initial time (a numeric scalar)}
##'   \item{time}{the final time (a numeric scalar)}
##'   \item{ndeme}{the number of demes (an integer)}
##'   \item{nsample}{the number of samples (an integer)}
##'   \item{newick}{the genealogical tree, in Newick format}
##'   \item{description}{a human readable description of the state of the genealogy process}
##'   \item{yaml}{the state of the genealogy process in YAML format}
##'   \item{structure}{the state of the genealogy process in \R list format}
##'   \item{lineages}{a \code{\link[tibble]{tibble}} containing the lineage count function through time}
##'   \item{genealogy}{the lineage-traced genealogy (as a raw vector)}
##' }
##' @example examples/siir.R
##' @rdname getinfo
##' @export
getInfo <- function (
  object, prune = TRUE, obscure = TRUE,
  t0 = FALSE, time = FALSE,
  description = FALSE, structure = FALSE, yaml = FALSE,
  ndeme = FALSE, lineages = FALSE, newick = FALSE,
  nsample = FALSE, genealogy = FALSE)
{
  x <- .External(P_getInfo,
    object=geneal(object),
    prune=prune,obscure=obscure,
    t0=t0,time=time,nsample=nsample,ndeme=ndeme,
    description=description,yaml=yaml,
    structure=structure,newick=newick,
    lineages=lineages,genealogy=genealogy
  )
  if (!is.null(x$lineages))
    x$lineages |> reshape_lineages() -> x$lineages
  x
}

reshape_lineages <- function (x) {
  x |> as_tibble() -> lin
  lin |>
    structure(
      class=c("gplin",class(lin))
    )
}

##' getInfo
##'
##' Retrieve information from genealogy process simulation
##'
##' @name getInfo
##' @param object \code{gpsim} object.
##' @param prune logical; prune the genealogy?
##' @param obscure logical; obscure the demes?
##' @param trace logical; trace the lineages?
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
##' @include package.R
##' @importFrom tibble as_tibble
##' @return
##' A list containing the requested elements, including any or all of:
##' \describe{
##'   \item{t0}{the initial time}
##'   \item{time}{the current time}
##'   \item{newick}{the genealogical tree, in Newick format}
##'   \item{description}{a human readable description of the state of the genealogy process}
##'   \item{yaml}{the state of the genealogy process in YAML format}
##'   \item{structure}{the state of the genealogy process in \R list format}
##'   \item{ndeme}{the number of demes (an integer)}
##'   \item{lineages}{a \code{\link[tibble]{tibble}} containing the lineage count function through time}
##'   \item{nsample}{the number of samples (an integer)}
##'   \item{genealogy}{the lineage-traced genealogy (as a raw vector)}
##' }
##' 
##' @example examples/siir.R
##' @rdname getinfo
##' @export
getInfo <- function (
  object, prune = TRUE, obscure = TRUE, trace = FALSE,
  t0 = FALSE, time = FALSE,
  description = FALSE, structure = FALSE, yaml = FALSE,
  ndeme = FALSE, lineages = FALSE, newick = FALSE,
  nsample = FALSE, genealogy = FALSE)
{
  x <- switch(
    paste0("model",as.character(attr(object,"model"))),
    modelSIR = .Call(P_infoSIR,object,prune,obscure,trace,t0,time,
      description,yaml,structure,ndeme,lineages,newick,nsample,genealogy),
    modelSIRS = .Call(P_infoSIR,object,prune,obscure,trace,t0,time,
      description,yaml,structure,ndeme,lineages,newick,nsample,genealogy),
    modelSEIR = .Call(P_infoSEIR,object,prune,obscure,trace,t0,time,
      description,yaml,structure,ndeme,lineages,newick,nsample,genealogy),
    modelSIIR = .Call(P_infoSIIR,object,prune,obscure,trace,t0,time,
      description,yaml,structure,ndeme,lineages,newick,nsample,genealogy),
    modelLBDP = .Call(P_infoLBDP,object,prune,obscure,trace,t0,time,
      description,yaml,structure,ndeme,lineages,newick,nsample,genealogy),
    modelMoran = .Call(P_infoMoran,object,prune,obscure,trace,t0,time,
      description,yaml,structure,ndeme,lineages,newick,nsample,genealogy),
    modelSI2R = .Call(P_infoSI2R,object,prune,obscure,trace,t0,time,
      description,yaml,structure,ndeme,lineages,newick,nsample,genealogy),
    modelBare = .Call(P_infoBare,object,prune,obscure,trace,t0,time,
      description,yaml,structure,ndeme,lineages,newick,nsample,genealogy),
    model = pStop("no model specified"),
    pStop("unrecognized model ",sQuote(attr(object,"model")))
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

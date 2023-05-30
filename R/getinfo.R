##' getInfo
##'
##' Retrieve information from genealogy process simulation
##'
##' @name getInfo
##' 
##' @param object \code{gpsim} object.
##' @param prune logical; prune the genealogy?
##' @param obscure logical; obscure the demes?
##' @param time logical; return the current time?
##' @param t0 logical; return the zero-time?
##' @param tree logical; return the tree?
##' @param description logical; return the description?
##' @param yaml logical; return the structure in YAML format?
##' @param structure logical; return the structure in \R list format?
##' @param lineages logical; return the lineage-count function?
##'
##' @include package.R
##' @importFrom tibble tibble
##'
##' @return
##' A list containing the requested elements, including any or all of:
##' \describe{
##'   \item{t0}{the initial time}
##'   \item{time}{the current time}
##'   \item{tree}{the genealogical tree, in Newick format}
##'   \item{description}{a human readable description of the state of the genealogy process}
##'   \item{yaml}{the state of the genealogy process in YAML format}
##'   \item{structure}{the state of the genealogy process in \R list format}
##'   \item{lineages}{a \code{\link[tibble]{tibble}} containing the lineage count function through time}
##' }
##' 
##' @example examples/siir.R
##'
##' @rdname getinfo
##' @export
getInfo <- function (
  object, prune = TRUE, obscure = TRUE,
  t0 = FALSE, time = FALSE,
  description = FALSE, structure = FALSE, yaml = FALSE,
  lineages = FALSE, tree = FALSE)
{
  x <- switch(
    paste0("model",as.character(attr(object,"model"))),
    modelSIR = .Call(P_infoSIR,object,prune,obscure,t0,time,
      description,yaml,structure,lineages,tree),
    modelSEIR = .Call(P_infoSEIR,object,prune,obscure,t0,time,
      description,yaml,structure,lineages,tree),
    modelSIIR = .Call(P_infoSIIR,object,prune,obscure,t0,time,
      description,yaml,structure,lineages,tree),
    modelLBDP = .Call(P_infoLBDP,object,prune,obscure,t0,time,
      description,yaml,structure,lineages,tree),
    modelMoran = .Call(P_infoMoran,object,prune,obscure,t0,time,
      description,yaml,structure,lineages,tree),
    modelSI2R = .Call(P_infoSI2R,object,prune,obscure,t0,time,
      description,yaml,structure,lineages,tree),
    model = pStop("no model specified"),
    pStop("unrecognized model ",sQuote(attr(object,"model")))
  )
  if (!is.null(x$tree)) x$tree <- gsub("nan","NA",x$tree)
  if (!is.null(x$lineages))
    x$lineages |> reshape_lineages() -> x$lineages
  x
}

reshape_lineages <- function (x) {
  ndeme <- length(unique(x$deme))
  tibble(
    time=rep(x$time,each=ndeme),
    deme=x$deme,
    lineages=x$lineages,
    saturation=x$saturation
  ) -> lin
  lin |>
    structure(
      class=c("gplin",class(lin))
    )
}

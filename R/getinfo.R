##' getInfo
##'
##' Retrieve information from genealogy process simulation
##'
##' @name getInfo
##'
##' @param object \code{gpsim} object.
##' @param prune logical; prune the genealogy?
##' @param obscure logical; obscure the demes?
##' @param hide logical; hide reassortment events?
##' @param time logical; return the current time?
##' @param t0 logical; return the zero-time?
##' @param tree logical; return the tree?
##' @param compact logical; return the tree in compact representation?
##' @param description logical; return the description?
##' @param yaml logical; return the structure in YAML format?
##' @param structure logical; return the structure in R list format?
##' @param lineages logical; return the lineage-count function?
##' @param gendat logical; return the genealogy as a data frame?
##'
##' @include package.R
##'
##' @return
##' A list containing the requested elements.
##'
##' @rdname getinfo
##' @export
getInfo <- function (
    object, prune  = TRUE, obscure = TRUE, hide = FALSE,
    t0 = FALSE, time = FALSE,
    description = FALSE,
    structure = FALSE, yaml = FALSE,
    lineages = FALSE,
    tree = FALSE, compact = TRUE,
    gendat = FALSE)
{
  if (gendat && !prune) {
    warning("pruning since 'gendat=TRUE'",call.=FALSE)
    prune <- TRUE
  }
  x <- switch(
    paste0("model",as.character(attr(object,"model"))),
    modelLBDPwr = .Call(P_infoLBDPwr2,object,prune,obscure,hide,t0,time,
                        description,yaml,structure,lineages,tree,compact,
                        gendat),
    modelSEI2Rwr = .Call(P_infoSEI2Rwr,object,prune,obscure,hide,t0,time,
                         description,yaml,structure,lineages,tree,compact,
                         gendat),
    model = stop("no model specified",call.=FALSE),
    stop("unrecognized model ",sQuote(attr(object,"model")),call.=FALSE)
  )
  if (!is.null(x$tree)) {
    x$tree <- gsub("nan","NA",x$tree)
  }
  x
}

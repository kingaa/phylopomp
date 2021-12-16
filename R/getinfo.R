##' Retrieve information from genealogy process simulation
##'
##' getInfo
##'
##' @name getInfo
##' 
##' @param data \code{gpsim} object.
##' @param ... arguments passed to specific methods.
##' @param prune logical; prune the tree?
##' @param compact logical; return the tree in compact representation?
##'
##' @include package.R
##' @importFrom tibble as_tibble
##' @importFrom yaml read_yaml
##'
##' @return
##' A list containing the following elements
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
getInfo <- function (data, ...) {
  UseMethod("getInfo",data)
}

##' @rdname getinfo
##' @export
getInfo.gpsim <- function (data, ..., prune  = TRUE, compact = TRUE) {
  x <- switch(
    attr(data,"model"),
    SIR = .Call(P_get_SIR_info,attr(data,"state"),prune,compact),
    SIIR = .Call(P_get_SIIR_info,attr(data,"state"),prune,compact),
    stop("unrecognized ",sQuote("gpsim")," object.",call.=FALSE)
  )
  x$tree <- gsub("nan","NA",x$tree)
  x$structure <- read_yaml(text=x$yaml)$tableau
  x$lineages <- as_tibble(x$lineages)
  attr(x,"model") <- attr(data,"model")
  attr(x,"state") <- attr(data,"state")
  class(x) <- c("gpsim",class(x))
  x
}

##' @importFrom yaml as.yaml
##' @docType import
##' @export
yaml::as.yaml

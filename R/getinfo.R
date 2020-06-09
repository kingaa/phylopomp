##' Retrieve information from genealogy process simulation
##'
##' getInfo
##'
##' @name getInfo
##' @aliases getinfo
##' 
##' @param data \code{gpsim} object.
##' @param ... arguments passed to specific methods.
##' @param prune logical; prune the tree?
##' @param tree logical; represent the genealogical tree in Newick format?
##'
##' @include package.R
##' 
##' @example examples/moran.R
##'
##' @rdname getinfo
##' @export
getInfo <- function (data, ...) {
  UseMethod("getInfo",data)
}

##' @export
getInfo.gpsim <- function (data, ..., prune  = TRUE, tree = TRUE) {
  x <- switch(
    attr(data,"model"),
    SIRwS = .Call(P_get_SIRwS_info,attr(data,"state"),prune,tree),
    Moran = .Call(P_get_Moran_info,attr(data,"state"),prune,tree),
    LBDP = .Call(P_get_LBDP_info,attr(data,"state"),prune,tree),
    stop("unrecognized ",sQuote("gpsim")," object.",call.=FALSE)
  )
  x$cumhaz <- tibble(
    time=x$stimes[-1],
    Lambda=if (length(x$cumhaz)>0) x$cumhaz else NA
  )
  x$lineages <- tibble(
    time=x$etimes,
    lineages=x$lineages
  )
  x$stimes <- NULL
  x$etimes <- NULL
  class(x) <- class(data)
  attr(x,"model") <- attr(data,"model")
  attr(x,"state") <- attr(data,"state")
  x
}

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
##'
##' @include package.R
##' @importFrom tibble as_tibble
##' 
##' @example examples/moran.R
##'
##' @rdname getinfo
##' @export
getInfo <- function (data, ...) {
  UseMethod("getInfo",data)
}

##' @rdname getinfo
##' @export
getInfo.gpsim <- function (data, ..., prune  = TRUE) {
  x <- switch(
    attr(data,"model"),
    SIRwS = .Call(P_get_SIRwS_info,attr(data,"state"),prune),
    Moran = .Call(P_get_Moran_info,attr(data,"state"),prune),
    LBDP = .Call(P_get_LBDP_info,attr(data,"state"),prune),
    stop("unrecognized ",sQuote("gpsim")," object.",call.=FALSE)
  )
  x$cumhaz <- as_tibble(x$cumhaz)
  x$lineages <- as_tibble(x$lineages)
  x$stimes <- NULL
  attr(x,"model") <- attr(data,"model")
  attr(x,"state") <- attr(data,"state")
  class(x) <- c("gpsim",class(x))
  x
}

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
getInfo.gpsim <- function (data, ..., prune  = TRUE, compact = FALSE) {
  x <- switch(
    attr(data,"model"),
    SIRS = .Call(P_get_SIRS_info,attr(data,"state"),prune,compact),
    LBDP = .Call(P_get_LBDP_info,attr(data,"state"),prune,compact),
    Moran = .Call(P_get_Moran_info,attr(data,"state"),prune,compact),
    SIRS = .Call(P_get_SIRS_info,attr(data,"state"),prune,compact),
    SIRwS = .Call(P_get_SIRwS_info,attr(data,"state"),prune,compact),
    multiSIRwS = .Call(P_get_multiSIRwS_info,attr(data,"state"),prune,compact),
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

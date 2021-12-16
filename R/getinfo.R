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
getInfo.gpsim <- function (data, ..., prune  = TRUE, compact = TRUE) {
  x <- switch(
    attr(data,"model"),
    SIR = .Call(P_get_SIR_info,attr(data,"state"),prune,compact),
    SIIR = .Call(P_get_SIIR_info,attr(data,"state"),prune,compact),
    stop("unrecognized ",sQuote("gpsim")," object.",call.=FALSE)
  )
  x$cumhaz <- as_tibble(x$cumhaz)
  x$lineages <- as_tibble(x$lineages)
  x$stimes <- NULL
  x$tree <- gsub("nan","NA",x$tree)
  attr(x,"model") <- attr(data,"model")
  attr(x,"state") <- attr(data,"state")
  class(x) <- c("gpsim",class(x))
  x
}

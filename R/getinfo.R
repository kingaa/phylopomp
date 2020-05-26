##' Retrieve information from genealogy process simulation
##'
##' getInfo
##'
##' @name getInfo
##' @aliases getinfo
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
getInfo.gpsim <- function (data, ...) {
  stop("unrecognized ",sQuote("gpsim")," object.",call.=FALSE)
}

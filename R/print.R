##' Special print functions
##'
##' Print functions for \sQuote{gpsim} objects.
##'
##' @name print
##' @keywords internal
##' 
##' @param x \code{gpsim} object.
##' @param ... arguments passed to print.
##'
##' @include getinfo.R
##' 
##' @rdname print
##' @export
print.gpsim <- function (x, ...) {
  attr(x,"state") <- NULL
  NextMethod("print")
}

##' @rdname internals
##' @keywords internals
##' @inheritParams base::print.default
##' @method print gpsim
##' @export
print.gpsim <- function (x, ...) {
  cat("<gpsim for",as.character(attr(x,"model")),"model>\n",sep=" ")
}

##' @rdname internals
##' @keywords internals
##' @method print gpgen
##' @export
print.gpgen <- function (x, ...) {
  cat("<phylopomp genealogy>\n")
}

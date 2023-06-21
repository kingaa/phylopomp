##' @rdname internals
##' @keywords internals
##' @inheritParams base::print.default
##' @method print gpsim
##' @export
print.gpsim <- function (x, ...) {
  if (is.null(attr(x,"model"))) {
    cat("<phylopomp genealogy>\n",sep=" ")
  } else {
    cat("<gpsim for",as.character(attr(x,"model")),"model>\n",sep=" ")
  }
}

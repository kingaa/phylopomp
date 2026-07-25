##' Compact, bijective, ladderized vector representation of a genealogy
##'
##' Computes the CBLV representation.
##'
##' @name cblv.R
##' @include package.R
##' @param object a \sQuote{gpgen} object, possibly with \sQuote{model} attribute.
##' @return A matrix with two columns.
##' @rdname cblv
##' @export
cblv <- function (object) {
  .Call(P_cblv,object)
}

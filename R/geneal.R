##' Bare genealogy
##'
##' Extracts the bare genealogy from a Markov genealogy process simulation
##'
##' @name geneal
##' @include package.R
##' @param object a \sQuote{gpgen} object, possibly with \sQuote{model} attribute.
##' @return A bare genealogy object.
##' @rdname geneal
##' @export
geneal <- function (object) {
  .Call(P_geneal,object)
}

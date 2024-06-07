##' Genealogy as a data frame
##'
##' Converts a given genealogy to a data frame.
##'
##' @name gendat
##' @include package.R
##' @param object a \sQuote{gpgen} object.
##' @return A list of objects containing the information pertinent for filtering.
##' @rdname gendat
##' @export
gendat <- function (object) {
  .Call(P_gendat,geneal(object))
}

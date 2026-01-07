##' Genealogy as a data frame
##'
##' Converts a given genealogy to a data frame.
##'
##' @name gendat
##' @include package.R
##' @param object a \sQuote{gpgen} object.
##' @inheritParams getInfo
##' @return A list of objects containing the information pertinent for filtering.
##' @rdname gendat
##' @export
gendat <- function (object, obscure = TRUE) {
  .Call(P_gendat,geneal(object),as.logical(obscure))
}

##' Genealogy as a data frame
##'
##' Converts a given genealogy to a data frame.
##'
##' @name gendat
##' @include package.R
##' @param object a \sQuote{gpgen} object.
##' @return A tibble.
##' @importFrom tibble as_tibble
##' @rdname gendat
##' @export
gendat <- function (object) {
  .Call(P_gendat,geneal(object)) |> as_tibble()
}

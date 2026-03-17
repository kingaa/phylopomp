##' Genealogy as a data frame
##'
##' Converts a given genealogy to data-frame format.
##' Returns a list of per-segment data frames.
##'
##' @name gendat
##' @include getinfo.R
##' @inheritParams getInfo
##' @return A list of per-segment data frames containing genealogy information.
##' @rdname gendat
##' @export
gendat <- function (object, obscure = TRUE) {
  getInfo(object,gendat=TRUE,prune=TRUE,obscure=obscure) |>
    getElement("gendat")
}

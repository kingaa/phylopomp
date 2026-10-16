##' Split a genealogy
##'
##' Splits a genealogy into individual trees.
##'
##' @name geneal_split
##' @include package.R getinfo.R sum.R
##' @param object a \sQuote{gpgen} object, possibly with \sQuote{model} attribute.
##' @inheritParams getInfo
##' @return \code{geneal_split} returns a list of bare genealogy objects, each one having a unique root.
##' @rdname geneal_sum
##' @export
geneal_split <- function (object, prune = TRUE, obscure = TRUE) {
  object |>
    getInfo(t0=TRUE,time=TRUE) -> info
  object |>
    geneal() |>
    newick(obscure=obscure,prune=prune,extended=TRUE) |>
    strsplit(";") |>
    getElement(1L) |>
    paste0(";") |>
    lapply(parse_newick,t0=info$t0,time=info$time)
}

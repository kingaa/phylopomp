##' Combine genealogies
##'
##' A summing operator for genealogies.
##'
##' @name geneal_sum
##' @return combined genealogy.
##' @example examples/sum.R
##' @details
##' The sum of genealogies is defined to be the union of all the lineages,
##' restricted to the common time-interval, i.e., the intersection of all the individual time-intervals.
##'
NULL

##' @rdname geneal_sum
##' @method + gpgen
##' @export
`+.gpgen` <- function (x, y) {
  geneal_sum(x,y)
}

##' @rdname geneal_sum
##' @param x,y,... genealogies or lists of genealogies to be summed.
##' @export
geneal_sum <- function (...) {
  do.call(geneal_add,lapply(list(...),geneal_flatten))
}

geneal_flatten <- function (x) {
  if (inherits(x,"gpgen"))
    x
  else if (is.list(x))
    do.call(geneal_sum,x)
  else
    pStop("can only be applied to genealogies or lists of genealogies.",who="geneal_sum")
}

geneal_add <- function (...)
  .External(P_genealSum,...)

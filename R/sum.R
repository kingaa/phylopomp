##' Combine genealogies
##'
##' A summing operator
##'
##' @name geneal_sum
##' @return combined genealogy.
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
  do.call(geneal_bind,lapply(list(...),geneal_flatten))
}

geneal_flatten <- function (x) {
  if (inherits(x,"gpgen"))
    geneal(x)
  else if (is.list(x)) {
    do.call(geneal_sum,x)
  } else {
    pStop("can only be applied to genealogies or lists of genealogies.",who="geneal_sum")
  }
}

geneal_bind <- function (...)
  .External(P_genealSum,...)

##' Subsample a genealogy
##'
##' Drops a random selection of samples
##'
##' @name subsample
##' @include package.R
##' @param object a \sQuote{gpgen} object, possibly with \sQuote{model} attribute.
##' @param frac fraction of samples to retain
##' @return A bare genealogy object containing the randomly subsampled genealogy.
##' @rdname subsample
##' @export
subsample <- function (object, frac) {
  .Call(P_subsample,object,as.double(frac))
}

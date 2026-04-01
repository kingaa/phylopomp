##' Curtail a genealogy to the given time
##'
##' Discards all nodes beyond the given time.
##'
##' @name curtail
##' @include getinfo.R
##' @inheritParams getInfo
##' @param t0,time numeric scalars; determine the time interval for curtailed genealogy
##' @return a genealogy object: the curtailed version of the input genealogy.
##' @example examples/curtail.R
##' @rdname curtail
##' @export
curtail <- function (object, time = NA, t0 = NA) {
  .Call(P_curtail,geneal(object),time,t0)
}

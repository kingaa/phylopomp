##' Curtail a genealogy to the given time
##'
##' Discards all nodes beyond the given time.
##'
##' @name curtail
##' @include getinfo.R
##' @inheritParams getInfo
##' @param troot new root time for curtailed genealogy
##' @return A curtailed genealogy object.
##' @example examples/curtail.R
##' @rdname curtail
##' @export
curtail <- function (object, time = NA, troot = NA) {
  .Call(P_curtail,geneal(object),time,troot)
}

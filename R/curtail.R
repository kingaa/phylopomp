##' Curtail a genealogy to the given time
##'
##' Discards all nodes beyond the given time.
##' Operates on a single bare genealogy (class \sQuote{gpgen}).
##'
##' @name curtail
##' @include geneal.R
##' @param object a \sQuote{gpgen} object (bare genealogy).
##' @param time new end time for curtailed genealogy.
##' @param troot new root time for curtailed genealogy.
##' @return A curtailed genealogy object (class \sQuote{gpgen}).
##' @rdname curtail
##' @export
curtail <- function (object, time = NA, troot = NA) {
  .Call(P_curtail,object,time,troot)
}

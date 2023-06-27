##' Curtail a genealogy to the given time
##'
##' Discards all nodes beyond the given time.
##'
##' @name curtail
##' @include getinfo.R
##' @inheritParams getInfo
##' @return A curtailed genealogy object.
##' @example examples/curtail.R
##' @rdname curtail
##' @export
curtail <- function (object, time = NA, prune = TRUE, obscure = TRUE) {
  .Call(P_curtail,geneal(object),time)
}

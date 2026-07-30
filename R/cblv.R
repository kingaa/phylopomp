##' Compact, bijective, ladderized vector representation of a genealogy
##'
##' Puts a genealogy into its CBLV representation and vice-versa.
##'
##' @name cblv
##' @include package.R
##' @param object a \sQuote{gpgen} object, possibly with \sQuote{model} attribute.
##' @return \code{cblv} returns a matrix with two columns.
##' @rdname cblv
##' @export
cblv <- function (object) {
  .Call(P_cblv,object)
}

##' @param xy CBLV representation, as a 2-column matrix
##' @param t0 root-time
##' @param time genealogy time
##' @return \code{parse_cblv} returns a \sQuote{gpgen} object.
##' @rdname cblv
##' @export
parse_cblv <- function (xy, t0, time) {
  storage.mode(xy) <- "double"
  .Call(P_parse_cblv,as.matrix(xy),as.double(t0),as.double(time)) |>
    structure(class="gpgen")
}

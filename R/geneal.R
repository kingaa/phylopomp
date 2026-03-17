##' Bare genealogy
##'
##' Extracts the bare per-segment genealogies from a
##' Markov genealogy process simulation.
##'
##' @name geneal
##' @include package.R
##' @param object a \sQuote{gpsim} object.
##' @return A list of bare genealogy objects (class \sQuote{gpgen}),
##'   one per segment.
##' @rdname geneal
##' @export
geneal <- function (object) {
  switch(
    paste0("model",as.character(attr(object,"model"))),
    modelLBDPwr = .Call(P_genealLBDPwr2,object),
    modelSEI2Rwr = .Call(P_genealSEI2Rwr,object),
    model = stop("no model specified",call.=FALSE),
    stop("unrecognized model ",sQuote(attr(object,"model")),call.=FALSE)
  )
}

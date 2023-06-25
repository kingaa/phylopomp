##' Bare genealogy
##'
##' Extracts the bare genealogy from a Markov genealogy process simulation
##'
##' @name geneal
##' @include package.R
##' @param object a \sQuote{gpsim} object.
##' @return A bare genealogy object.
##' @rdname geneal
##' @export
geneal <- function (object) {
  switch(
    paste0("model",as.character(attr(object,"model"))),
    modelSIR = .Call(P_genealSIR,object),
    modelSIRS = .Call(P_genealSIR,object),
    modelSEIR = .Call(P_genealSEIR,object),
    modelSIIR = .Call(P_genealSIIR,object),
    modelLBDP = .Call(P_genealLBDP,object),
    modelMoran = .Call(P_genealMoran,object),
    modelSI2R = .Call(P_genealSI2R,object),
    model = object,
    pStop("unrecognized model ",sQuote(attr(object,"model")))
  ) |>
    structure(class="gpsim")
}

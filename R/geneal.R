##' Bare genealogy
##'
##' Extracts the bare genealogy from a Markov genealogy process simulation
##'
##' @name geneal
##' @include package.R
##' @param object a \sQuote{gpgen} object.
##' @return A bare genealogy object.
##' @rdname geneal
##' @export
geneal <- function (object) {
  switch(
    paste0("model",as.character(attr(object,"model"))),
    modelLBDP = .Call(P_genealLBDP,object),
    modelMoran = .Call(P_genealMoran,object),
    modelS2I2R2 = .Call(P_genealS2I2R2,object),
    modelSEIR = .Call(P_genealSEIR,object),
    modelSI2R = .Call(P_genealSI2R,object),
    modelSIIR = .Call(P_genealSIIR,object),
    modelSIR = .Call(P_genealSIR,object),
    modelStrains = .Call(P_genealStrains,object),
    modelTwoSpecies = .Call(P_genealTwoSpecies,object),
    model = structure(object,class=c("gpgen")),
    pStop("unrecognized model ",sQuote(attr(object,"model")))
  )
}

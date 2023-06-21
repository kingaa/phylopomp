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
  time <- as.numeric(time)
  switch(
    paste0("model",as.character(attr(object,"model"))),
    modelSIR = .Call(P_curtailSIR,object,time),
    modelSIRS = .Call(P_curtailSIR,object,time),
    modelSEIR = .Call(P_curtailSEIR,object,time),
    modelSIIR = .Call(P_curtailSIIR,object,time),
    modelLBDP = .Call(P_curtailLBDP,object,time),
    modelMoran = .Call(P_curtailMoran,object,time),
    modelSI2R = .Call(P_curtailSI2R,object,time),
    model = .Call(P_curtailBare,object,time),
    pStop("unrecognized model ",sQuote(attr(object,"model")))
  ) |>
    structure(model=attr(object,"model"),class="gpsim")
}

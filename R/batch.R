##' batch
##'
##' Batch sampling at the end of process
##'
##' @name batch
##' @include getinfo.R lbdp.R sir.R seir.R moran.R siir.R si2r.R lbdpwr.R moranwr.R sirwr.R
##' 
##' @family Genealogy processes
##' 
##' @param object either the name of the model to end with batch sampling
##' \emph{or} a previously computed \sQuote{gpsim} object
##'
##' @return An object of \sQuote{gpsim} class.
##'
NULL

##' @rdname batch
##' @export
##' 
batch <- function (object) {
  model <- as.character(attr(object,"model"))
  switch(
    paste0("model",model),
    modelSIR = .Call(P_batchSIR,object),
    modelSIIR = .Call(P_batchSIIR,object),
    modelLBDP = .Call(P_batchLBDP,object),
    modelMoran = .Call(P_batchMoran,object),
    modelSI2R = .Call(P_batchSI2R,object),
    modelSEIR = .Call(P_batchSEIR,object),
    modelSIRwr = .Call(P_batchSIRwr,object),
    modelLBDPwr = .Call(P_batchLBDPwr,object),
    modelMoranwr = .Call(P_batchMoranwr,object),
    model = stop("no model specified",call.=FALSE),
    stop("unrecognized model ",sQuote(attr(object,"model")),call.=FALSE)
  ) |>
    structure(model=model,class="gpsim")
}

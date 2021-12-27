##' simulate
##'
##' Simulate Markov genealogy processes
##'
##' @name simulate
##' @include getinfo.R sir.R siir.R si2r.R lbdp.R moran.R
##' 
##' @family Genealogy processes
##' 
##' @param object either the name of the model to simulate
##' \emph{or} a previously computed \sQuote{gpsim} object
##' @param ... additional arguments to the model-specific simulation functions
##'
##' @return An object of \sQuote{gpsim} class.
##'
NULL

##' @rdname simulate
##' @export
simulate <- function (object, ...) {
  UseMethod("simulate")
}

##' @rdname simulate
##' @method simulate default
##' @export
simulate.default <- function (object, ...) {
  if (missing(object) || is.null(object))
    message(
      "Available phylopomp models:\n",
      "- SIR: standard susceptible-infected-recovered model\n",
      "- SIIR: two-strain SIR model\n",
      "- SI2R: superspreading model\n",
      "- Moran: Moran process\n",
      "- LBDP: linear birth-death-sampling process\n"
    )
  else
    stop(
      "in ",sQuote("phylopomp::simulate"),": ",
      sQuote("object")," must be specified as either ",
      "the name of a model or the result of a previous simulation.\n",
      "Do ",sQuote("simulate()")," to view available models.",
      call.=FALSE
    )  
}

##' @rdname simulate
##' @method simulate character
##' @param time end timepoint of simulation
##' @export
simulate.character <- function (object, time, ...) {
  switch(
    object,
    SIR = runSIR(time=time,...),
    SIIR = runSIIR(time=time,...),
    SI2R = runSI2R(time=time,...),
    LBDP = runLBDP(time=time,...),
    Moran = runMoran(time=time,...),
    stop("unrecognized model: ",sQuote(object),".",
      "Do ",sQuote("simulate()")," to view available models.",
      call.=FALSE)
  ) |>
    structure(model=object,class="gpsim")
}

##' @rdname simulate
##' @method simulate gpsim
##' @details
##' When \code{object} is of class \sQuote{gpsim}, i.e., the result of a genealogy-process
##' simulation, \code{simulate} acts to continue the simulation to a later timepoint.
##' Note that, one cannot change initial conditions or \code{t0} when continuing a simulation.
##' 
##' @export
simulate.gpsim <- function (object, time, ...) {
  model <- as.character(attr(object,"model"))
  switch(
    paste0("model",model),
    modelSIR = continueSIR(object,time=time,...),
    modelSIIR = continueSIIR(object,time=time,...),
    modelSI2R = continueSI2R(object,time=time,...),
    modelLBDP = continueLBDP(object,time=time,...),
    modelMoran = continueMoran(object,time=time,...),
    model = stop("no model attribute detected.",call.=FALSE),
    stop("unrecognized model ",sQuote(model),".",call.=FALSE)
  ) |>
    structure(model=model,class="gpsim")
}

##' simulate
##'
##' Simulate Markov genealogy processes
##'
##' @name simulate
##' @include getinfo.R seir.R sir.R siir.R si2r.R lbdp.R moran.R
##' @family Genealogy processes
##' @param object either the name of the model to simulate
##' \emph{or} a previously computed \sQuote{gpsim} object
##' @param ... additional arguments to the model-specific simulation functions
##' @return An object of \sQuote{gpsim} class.
##' @references
##' \King2024
##'
##' \King2022
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
      "- SIR: standard susceptible-infected-recovered model\n    with optional waning of immunity\n",
      "- SIRS: synonymous with SIR\n",
      "- SEIR: standard susceptible-exposed-infected-recovered model\n",
      "- SEIRS: synonymous with SEIR\n",
      "- SIIR: two-strain SIR model\n",
      "- SI2R: superspreading model\n",
      "- S2I2R2: two-species model\n",
      "- Moran: Moran process\n",
      "- LBDP: linear birth-death-sampling process\n"
    )
  else
    pStop(
      sQuote("object")," must be specified as either ",
      "the name of a model or the result of a previous simulation.\n",
      "Do ",sQuote("simulate()")," to view available models."
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
    SIRS = runSIRS(time=time,...),
    SEIR = runSEIR(time=time,...),
    SEIRS = runSEIRS(time=time,...),
    S2I2R2 = runS2I2R2(time=time,...),
    SIIR = runSIIR(time=time,...),
    SI2R = runSI2R(time=time,...),
    LBDP = runLBDP(time=time,...),
    Moran = runMoran(time=time,...),
    pStop_("unrecognized model: ",sQuote(object),".\n",
      "Do ",sQuote("simulate()")," to view available models.")
  ) |>
    structure(model=object,class=c("gpsim","gpgen"))
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
    modelSIRS = continueSIR(object,time=time,...),
    modelSEIR = continueSEIR(object,time=time,...),
    modelSEIRS = continueSEIRS(object,time=time,...),
    modelS2I2R2 = continueS2I2R2(object,time=time,...),
    modelSIIR = continueSIIR(object,time=time,...),
    modelSI2R = continueSI2R(object,time=time,...),
    modelLBDP = continueLBDP(object,time=time,...),
    modelMoran = continueMoran(object,time=time,...),
    model = pStop_("no model attribute detected."),
    pStop_("unrecognized model ",sQuote(model),".")
  ) |>
    structure(model=model,class=c("gpsim","gpgen"))
}

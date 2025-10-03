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
      "- LBDP: Linear birth-death-sampling model\n",
      "- Moran: The classical Moran model\n",
      "- S2I2R2: Two-host infection model with waning, immigration, and demography.\n",
      "- SEIR: Classical susceptible-exposed-infected-recovered model\n",
      "- SI2R: Two-deme model of superspreading\n",
      "- SIIR: Two-strain SIR model\n",
      "- SIR: Classical susceptible-infected-recovered model\n",
      "- Strains: Three strains compete for a single susceptible pool.\n",
      "- TwoSpecies: Two-host infection model with waning, immigration, demography, and spillover. Hosts are culled upon sampling with a given probability.\n",
      "- TwoUndead: Two-host infection model with waning, immigration, demography, and spillover. Hosts are culled upon sampling with a given probability. This is identical to the TwoSpecies model with the exception that dead lineages are not pruned. Instead, they become *ghosts*.\n",
      "- SIRS: synonymous with SIR\n",
      "- SEIRS: synonymous with SEIR\n",
      "\n"
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
    paste0("model",object),
    modelLBDP = runLBDP(time=time,...),
    modelMoran = runMoran(time=time,...),
    modelS2I2R2 = runS2I2R2(time=time,...),
    modelSEIR = runSEIR(time=time,...),
    modelSI2R = runSI2R(time=time,...),
    modelSIIR = runSIIR(time=time,...),
    modelSIR = runSIR(time=time,...),
    modelStrains = runStrains(time=time,...),
    modelTwoSpecies = runTwoSpecies(time=time,...),
    modelTwoUndead = runTwoUndead(time=time,...),
    modelSIRS = runSIR(time=time,...),
    modelSEIRS = runSEIR(time=time,...),
    pStop_("unrecognized model: ",sQuote(object),".\n",
      "Do ",sQuote("simulate()")," to view available models.")
  )
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
    modelLBDP = continueLBDP(object,time=time,...),
    modelMoran = continueMoran(object,time=time,...),
    modelS2I2R2 = continueS2I2R2(object,time=time,...),
    modelSEIR = continueSEIR(object,time=time,...),
    modelSI2R = continueSI2R(object,time=time,...),
    modelSIIR = continueSIIR(object,time=time,...),
    modelSIR = continueSIR(object,time=time,...),
    modelStrains = continueStrains(object,time=time,...),
    modelTwoSpecies = continueTwoSpecies(object,time=time,...),
    modelTwoUndead = continueTwoUndead(object,time=time,...),
    model = pStop_("no model attribute detected."),
    pStop_("unrecognized model ",sQuote(model),".")
  )
}

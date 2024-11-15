##' Get population state history
##'
##' Retrieves the history of population states (compartment sizes over time)
##' from a Markov genealogy process simulation
##'
##' @name get_states
##' @param object A \sQuote{gpsim} object
##' @return A \code{\link[tibble]{tibble}} containing the population state history
##' @export
get_states <- function(object) {
  if (!inherits(object, "gpsim")) {
    stop("object must be a gpsim object")
  }

  switch(
    paste0("model", as.character(attr(object, "model"))),
    modelLBDP = .Call(P_get_states_LBDP,object),
    modelMoran = .Call(P_get_states_Moran,object),
    modelS2I2R2 = .Call(P_get_states_S2I2R2,object),
    modelSEIR = .Call(P_get_states_SEIR,object),
    modelSI2R = .Call(P_get_states_SI2R,object),
    modelSIIR = .Call(P_get_states_SIIR,object),
    modelSIR = .Call(P_get_states_SIR,object),
    modelTIMVA = .Call(P_get_states_TIMVA,object),
    modelTwoSpecies = .Call(P_get_states_TwoSpecies,object),
    stop("unrecognized model ", sQuote(attr(object, "model")))
  ) |>
    tibble::as_tibble()
}

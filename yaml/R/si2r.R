##' Two-deme model of superspreading
##' 
##' Deme 1 consists of "ordinary infections" that transmit at rate code{Beta}. Deme 2 consists of "superspreaders" who engender clusters of infection in "superspreading events".
##' 
##' @name si2r
##' @family Genealogy processes
##' @aliases SI2R
##' @param Beta transmission rate
##' @param mu mean superspreading-event cluster size
##' @param gamma recovery rate
##' @param delta rate of waning of immunity
##' @param psi1 sampling rate for deme 1
##' @param psi2 sampling rate for deme 2
##' @param sigma12 rate of movement from deme 1 to deme 2
##' @param sigma21 rate of movement from deme 2 to deme 1
##' @param S0 initial size of susceptible population
##' @param I0 initial size of deme-1 infected population
##' @param R0 initial size of immune population
##'
##' @return \code{runSI2R} and \code{continueSI2R} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SI2R}.
##' 
NULL

##' @rdname si2r
##' @export
runSI2R <- function (
  time, t0 = 0,
  Beta = 5, mu = 5, gamma = 1, delta = 0, psi1 = 1, psi2 = 0, sigma12 = 1, sigma21 = 3, S0 = 500, I0 = 10, R0 = 0
) {
  params <- c(Beta=Beta,mu=mu,gamma=gamma,delta=delta,psi1=psi1,psi2=psi2,sigma12=sigma12,sigma21=sigma21)
  ivps <- c(S0=S0,I0=I0,R0=R0)
  x <- .Call(P_makeSI2R,params,ivps,t0)
  .Call(P_runSI2R,x,time) |>
    structure(model="SI2R",class="gpsim")
}

##' @rdname si2r
##' @export
continueSI2R <- function (
  object, time,
  Beta = NA, mu = NA, gamma = NA, delta = NA, psi1 = NA, psi2 = NA, sigma12 = NA, sigma21 = NA
) {
  params <- c(
    Beta=Beta,mu=mu,gamma=gamma,delta=delta,psi1=psi1,psi2=psi2,sigma12=sigma12,sigma21=sigma21
  )
  x <- .Call(P_reviveSI2R,object,params)
  .Call(P_runSI2R,x,time) |>
    structure(model="SI2R",class="gpsim")
}
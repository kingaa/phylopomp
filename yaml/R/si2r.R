##' Two-deme model of superspreading
##'
##' Deme L consists of "low-rate spreaders" that transmit at rate \code{Beta}.
##' Deme H consists of "superspreaders" who transmit at a higher rate
##' \code{kappa*Beta}.
##'
##' @name si2r
##' @family Genealogy processes
##' @aliases SI2R
##' @include getinfo.R
##' @param Beta transmission rate
##' @param kappa super-spreading transmission factor
##' @param gamma recovery rate
##' @param omega rate of waning of immunity
##' @param psi sampling rate
##' @param etaL rate of transition from low-spreading to super-spreading behavior
##' @param etaH rate of transition from super-spreading to low-spreading behavior
##' @param pop population size
##' @param S0 initial size of susceptible population
##' @param IL0 initial size of low-spreading population
##' @param IH0 initial size of super-spreading population
##' @param R0 initial size of immune population
##' @inheritParams sir
##' @return \code{runSI2R} and \code{continueSI2R} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SI2R}.
##'
NULL

##' @rdname si2r
##' @export
runSI2R <- function (
  time, t0 = 0,
  Beta = 5, kappa = 2, gamma = 1, omega = 0, psi = 1, etaL = 1, etaH = 3, pop = 500, S0 = 0.98, IL0 = 0.02, IH0 = 0, R0 = 0
) {
  params <- c(Beta=Beta,kappa=kappa,gamma=gamma,omega=omega,psi=psi,etaL=etaL,etaH=etaH)
  ivps <- c(pop=pop,S0=S0,IL0=IL0,IH0=IH0,R0=R0)
  x <- .Call(P_makeSI2R,params,ivps,t0)
  .Call(P_runSI2R,x,time) |>
    structure(model="SI2R",class=c("gpsim","gpgen"))
}

##' @rdname si2r
##' @export
continueSI2R <- function (
  object, time,
  Beta = NA, kappa = NA, gamma = NA, omega = NA, psi = NA, etaL = NA, etaH = NA
) {
  params <- c(
    Beta=Beta,kappa=kappa,gamma=gamma,omega=omega,psi=psi,etaL=etaL,etaH=etaH
  )
  x <- .Call(P_reviveSI2R,object,params)
  .Call(P_runSI2R,x,time) |>
    structure(model="SI2R",class=c("gpsim","gpgen"))
}
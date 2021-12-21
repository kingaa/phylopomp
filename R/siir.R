##' SIIR with sampling simulator.
##'
##' Run the simulator.
##'
##' @name siir
##' @aliases SIIR
##' @include getinfo.R
##' 
##' @family Genealogy processes
##' 
##' @inheritParams sir
##' @param Beta1,Beta2 transmission rates from each of the infectious classes.
##' @param gamma recovery rate.
##' @param psi1,psi2 sampling rates.
##' @param sigma12,sigma21 movement rates from deme 1 to 2 and 2 to 1, respectively
##' @param S0 initial size of susceptible population.
##' @param I1_0 initial size of I2 population.
##' @param I2_0 initial size of I2 population.
##' @param R0 initial size of recovered population.
##' 
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{"SIIR"}.
##'
##' @example examples/siir.R
##'
NULL

##' @rdname siir
##' @export
runSIIR <- function (
  time, t0 = 0,
  Beta1 = 1, Beta2 = 50, gamma = 1,
  psi1 = 1, psi2 = psi1,
  sigma12 = 1, sigma21 = 9,
  S0 = 100, I1_0 = 10, I2_0 = 0, R0 = 0
) {
  params <- c(Beta1=Beta1,Beta2=Beta2,gamma=gamma,
    psi1=psi1,psi2=psi2,sigma12=sigma12,sigma21=sigma21)
  ics <- c(S0=S0,I1_0=I1_0,I2_0=I2_0,R0=R0)
  x <- .Call(P_makeSIIR,params,ics,t0)
  x <- .Call(P_runSIIR,x,time)
  attr(x,"model") <- "SIIR"
  class(x) <- c("gpsim",class(x))
  x
}

##' @rdname siir
##' @inheritParams simulate
##' @export
continueSIIR <- function (
  object, time,
  Beta1 = NA, Beta2 = NA, gamma = NA,
  psi1 = NA, psi2 = NA,
  sigma21 = NA, sigma12 = NA
) {
  params <- c(Beta1=Beta1,Beta2=Beta2,gamma=gamma,
    psi1=psi1,psi2=psi2,sigma12=sigma12,sigma21=sigma21)
  x <- .Call(P_reviveSIIR,object,params)
  x <- .Call(P_runSIIR,x,time)
  x
}

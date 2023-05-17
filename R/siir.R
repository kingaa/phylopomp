##' Two-strain SIR model.
##'
##' Two distinct pathogen strains compete for susceptibles.
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
##' @param delta rate of loss of immunity
##' @param S0 initial size of susceptible population.
##' @param I1_0 initial size of I2 population.
##' @param I2_0 initial size of I2 population.
##' @param R0 initial size of recovered population.
##' @return \code{runSIIR} and \code{continueSIIR} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SIIR}.
##'
##' @example examples/siir.R
##'
NULL

##' @rdname siir
##' @export
runSIIR <- function (
  time, t0 = 0,
  Beta1 = 5, Beta2 = 5, gamma = 1,
  psi1 = 1, psi2 = 0,
  sigma12 = 0, sigma21 = 0,
  delta = 0,
  S0 = 500, I1_0 = 10, I2_0 = 10, R0 = 0
) {
  params <- c(Beta1=Beta1,Beta2=Beta2,gamma=gamma,psi1=psi1,psi2=psi2,sigma12=sigma12,sigma21=sigma21,delta=delta)
  ivps <- c(S0=S0,I1_0=I1_0,I2_0=I2_0,R0=R0)
  x <- .Call(P_makeSIIR,params,ivps,t0)
  .Call(P_runSIIR,x,time) |>
    structure(model="SIIR",class="gpsim")
}

##' @rdname siir
##' @export
continueSIIR <- function (
  object, time,
  Beta1 = NA, Beta2 = NA, gamma = NA,
  psi1 = NA, psi2 = NA,
  sigma12 = NA, sigma21 = NA,
  delta = NA
) {
  params <- c(
    Beta1=Beta1,Beta2=Beta2,gamma=gamma,psi1=psi1,psi2=psi2,sigma12=sigma12,sigma21=sigma21,delta=delta
  )
  x <- .Call(P_reviveSIIR,object,params)
  .Call(P_runSIIR,x,time) |>
    structure(model="SIIR",class="gpsim")
}

##' Two-class SEIRS model with reassortment
##'
##' The genealogy process induced by a two-class SEIRS model
##' with two segments reassorted: HA and NA
##'
##' @name sei2rwr
##' @aliases SEI2Rwr
##' @include getinfo.R
##'
##' @family Genealogy processes
##'
##' @param Beta0 per-capita ordinary transmission rate
##' @param sigma per-capita progression rate
##' @param gamma per-capita recovery rate
##' @param psi per-capita sampling rate
##' @param omega per-capita immunity decay rate
##' @param rhoA per-capita reassortment rate for segment A
##' @param rhoB per-capita reassortment rate for segment B
##' @param pr12 probability of migration from ordinary to high-transmissible
##' @param pr21 probability of migration from high-transmissible to ordinary
##' @param rr high-transmissible ratio over ordinary
##' @param p probability of sample termination
##' @param S0,E0,I0,R0 initial sizes of S, E1, I1, R compartments, respectively.
##' @param time final time
##' @param t0 initial time
##'
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SEI2Rwr}.
##'
NULL

##' @rdname sei2rwr
##' @export
runSEI2Rwr <- function (
  time, t0 = 0,
  Beta0 = 4, sigma = 1, gamma = 1, psi = 1, omega = 0,
  rhoA = .1, rhoB = 0, pr12 = .2, pr21 = 0, rr = 1.5,
  p = 1,
  S0 = 100, E0 = 5, I0 = 5, R0 = 0
) {
  c(
    Beta0=Beta0, sigma=sigma, gamma=gamma, psi=psi, omega=omega,
    rhoA=rhoA, rhoB=rhoB, pr12=pr12, pr21=pr21, rr=rr, p=p
  ) -> params
  ivps <- c(S0=S0, E0=E0, I0=I0, R0=R0)
  if (any(ivps < 0))
    pStop(paste(sQuote(names(ivps)),collapse=","),
          " must be nonnegative integers.")
  x <- .Call(P_makeSEI2Rwr,params,ivps,t0)
  x <- .Call(P_runSEI2Rwr,x,time)
  structure(x,model="SEI2Rwr",class="gpsim")
}

##' @rdname sei2rwr
##' @inheritParams simulate
##' @export
continueSEI2Rwr <- function (
  object, time,
  Beta0 = NA, sigma = NA, gamma = NA, psi = NA, omega = NA,
  rhoA = NA, rhoB = NA, pr12 = NA, pr21 = NA, rr = NA,
  p = NA
) {
  c(
    Beta0=Beta0, sigma=sigma, gamma=gamma, psi=psi, omega=omega,
    rhoA=rhoA, rhoB=rhoB, pr12=pr12, pr21=pr21, rr=rr, p=p
  ) -> params
  x <- .Call(P_reviveSEI2Rwr,object,params)
  x <- .Call(P_runSEI2Rwr,x,time)
  structure(x,model="SEI2Rwr",class="gpsim")
}

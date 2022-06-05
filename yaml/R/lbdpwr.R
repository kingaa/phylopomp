##' Linear birth-death-sampling model
##'
##' The genealogy process induced by a simple linear birth-death process
##' with constant-rate sampling.
##'
##' @name lbdpwr
##' @aliases LBDPwr
##' @include getinfo.R
##'
##' @family Genealogy processes
##'
##' @param lambda per capita birth rate
##' @param mu per capita recovery rate.
##' @param psi per capita sampling rate.
##' @param rhoA per capita reassortment rate for segment A
##' @param rhoB per capita reassortment rate for segment B
##' @param frac fraction of batch sampling
##' @param n0 initial population size
##' @param time final time
##' @param t0 initial time
##'
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{LBDPwr}.
##'
##' @example examples/lbdpwr.R
NULL

##' @rdname lbdpwr
##' @export
runLBDPwr <- function (
  time,  t0 = 0,
  lambda = 2, mu = 1, psi = 1,
  rhoA = 0, rhoB=0, frac = 0,
  n0 = 5
) {
  params <- c(lambda=lambda,mu=mu,psi=psi,rhoA=rhoA,rhoB=rhoB,frac=frac)
  ivps <- c(n0=n0)
  x <- .Call(P_makeLBDPwr,params,ivps,t0)
  x <- .Call(P_runLBDPwr,x,time)
  structure(x,model="LBDPwr",class="gpsim")
}

##' @rdname lbdpwr
##' @inheritParams simulate
##' @export
continueLBDPwr <- function (
  object, time, lambda = NA, mu = NA, psi = NA,
  rhoA = NA, rhoB = NA, frac = NA
) {
  params <- c(lambda=lambda,mu=mu,psi=psi,rhoA=rhoA,rhoB=rhoB,frac=frac)
  x <- .Call(P_reviveLBDPwr,object,params)
  .Call(P_runLBDPwr,x,time)
}

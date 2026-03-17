##' Linear birth-death-sampling model with reassortment
##'
##' The genealogy process induced by a simple linear birth-death process
##' with two segments reassorted
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
##' @param p probability of sample termination (0 = lineage continues, 1 = lineage dies)
##' @param n0 initial population size
##' @param time final time
##' @param t0 initial time
##'
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{LBDPwr}.
##'
NULL

##' @rdname lbdpwr
##' @export
runLBDPwr <- function (
  time, t0 = 0,
  lambda = 2, mu = 1, psi = 1,
  rhoA = 0, rhoB = 0, p = 1,
  n0 = 5
) {
  params <- c(lambda=lambda,mu=mu,psi=psi,rhoA=rhoA,rhoB=rhoB,p=p)
  ivps <- c(n0=n0)
  x <- .Call(P_makeLBDPwr2,params,ivps,t0)
  x <- .Call(P_runLBDPwr2,x,time)
  structure(x,model="LBDPwr",class="gpsim")
}

##' @rdname lbdpwr
##' @inheritParams simulate
##' @export
continueLBDPwr <- function (
  object, time, lambda = NA, mu = NA, psi = NA,
  rhoA = NA, rhoB = NA, p = NA
) {
  params <- c(lambda=lambda,mu=mu,psi=psi,rhoA=rhoA,rhoB=rhoB,p=p)
  x <- .Call(P_reviveLBDPwr2,object,params)
  x <- .Call(P_runLBDPwr2,x,time)
  structure(x,model="LBDPwr",class="gpsim")
}

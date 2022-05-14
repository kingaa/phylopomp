##' SIR model with S, M, L segments and reassortment
##'
##' A single, unstructured population of hosts.
##'
##' @name sirwr
##' @aliases SIRwr
##' @include getinfo.R
##' 
##' @family Genealogy processes
##'
##' @param Beta transmission rate.
##' @param gamma recovery rate.
##' @param psi sampling rate.
##' @param delta immunity waning rate
##' @param rhoS reassortment rate of S segment
##' @param rhoM reassortment rate of M segment
##' @param rhoL reassortment rate of L segment
##' @param rhoSM reassortment rate of both S and M segment
##' @param rhoSL reassortment rate of both S and L segment
##' @param rhoML reassortment rate of both M and L segment
##' @param S0 initial size of susceptible population.
##' @param I0 initial size of infected population.
##' @param R0 initial size of recovered population.
##' @param time final time
##' @param t0 initial time
##' 
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SIR}.
##'
##' @example examples/sirwr.R
NULL

##' @rdname sirwr
##' @export
runSIRwr <- function (
  time,  t0 = 0, 
  Beta = 2, gamma = 1, psi = 1, delta = 0,
  rhoS = 0, rhoM = .1, rhoL = 0, 
  rhoSM = 0, rhoSL = 0, rhoML = 0,
  S0 = 100, I0 = 2, R0 = 0
) {
  params <- c(Beta=Beta,gamma=gamma,psi=psi,delta=delta,
    rhoS=rhoS,rhoM=rhoM,rhoL=rhoL,
    rhoSM=rhoSM, rhoSL=rhoSL, rhoML=rhoML)
  ivps <- c(S0=S0,I0=I0,R0=R0)
  x <- .Call(P_makeSIRwr,params,ivps,t0)
  x <- .Call(P_runSIRwr,x,time)
  structure(x,model="SIRwr",class="gpsim")
}

##' @rdname sirwr
##' @inheritParams simulate
##' @export
continueSIRwr <- function (
  object, time, Beta = NA, gamma = NA, psi = NA, delta = NA,
  rhoS = NA, rhoM = NA, rhoL = NA, 
  rhoSM = NA, rhoSL = NA, rhoML = NA
) {
  params <- c(Beta=Beta,gamma=gamma,psi=psi,delta=delta,
    rhoS=rhoS,rhoM=rhoM,rhoL=rhoL,
    rhoSM=rhoSM,rhoSL=rhoSL,rhoML=rhoML)
  x <- .Call(P_reviveSIRwr,object,params)
  .Call(P_runSIRwr,x,time)
}

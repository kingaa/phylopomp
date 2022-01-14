##' Classical susceptible-infected-recovered model
##' 
##' A single, unstructured population of hosts.
##' 
##' @name sir
##' @family Genealogy processes
##' @aliases SIR
##' @param Beta1 transmission rate
##' @param gamma recovery rate
##' @param psi per capita sampling rate
##' @param delta rate of waning of immunity
##' @param S0 initial size of susceptible population
##' @param I0 initial size of infected population
##' @param R0 initial size of immune population
##'
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SIR}.
##' 
NULL

##' @rdname sir
##' @export
runSIR <- function (
  time, t0 = 0,
  Beta1 = 4, gamma = 1, psi = 1, delta = 0, S0 = 100, I0 = 5, R0 = 0
) {
  params <- c(Beta1=Beta1,gamma=gamma,psi=psi,delta=delta)
  ivps <- c(S0=S0,I0=I0,R0=R0)
  x <- .Call(P_makeSIR,params,ivps,t0)
  x <- .Call(P_runSIR,x,time)
  structure(x,model="SIR",class="gpsim")
}

##' @rdname sir
##' @export
continueSIR <- function (
  object, time,
  Beta1 = NA, gamma = NA, psi = NA, delta = NA
) {
  params <- c(
    Beta1=Beta1,gamma=gamma,psi=psi,delta=delta
  )
  x <- .Call(P_reviveSIR,object,params)
  .Call(P_runSIR,x,time)
}
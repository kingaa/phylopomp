##' Classical susceptible-infected-recovered model
##'
##' A single, unstructured population of hosts.
##'
##' @name sir
##' @family Genealogy processes
##' @aliases SIR
##' @include getinfo.R
##' @param Beta transmission rate
##' @param gamma recovery rate
##' @param psi per capita (nondestructive) sampling rate
##' @param chi per capita (destructive) sampling rate
##' @param omega rate of waning of immunity
##' @param pop size of population
##' @param S0 initial size of susceptible population
##' @param I0 initial size of infected population
##' @param R0 initial size of immune population
##' @param object a previously computed simulation
##' @param time end timepoint of simulation
##' @param t0 beginning timepoint of simulation
##' @return \code{runSIR} and \code{continueSIR} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SIR}.
##'
NULL

##' @rdname sir
##' @export
runSIR <- function (
  time, t0 = 0,
  Beta = 4, gamma = 1, psi = 1, chi = 0, omega = 0, pop = 100, S0 = 0.95, I0 = 0.05, R0 = 0
) {
  params <- c(Beta=Beta,gamma=gamma,psi=psi,chi=chi,omega=omega)
  ivps <- c(pop=pop,S0=S0,I0=I0,R0=R0)
  x <- .Call(P_makeSIR,params,ivps,t0)
  .Call(P_runSIR,x,time) |>
    structure(model="SIR",class=c("gpsim","gpgen"))
}

##' @rdname sir
##' @export
continueSIR <- function (
  object, time,
  Beta = NA, gamma = NA, psi = NA, chi = NA, omega = NA
) {
  params <- c(
    Beta=Beta,gamma=gamma,psi=psi,chi=chi,omega=omega
  )
  x <- .Call(P_reviveSIR,object,params)
  .Call(P_runSIR,x,time) |>
    structure(model="SIR",class=c("gpsim","gpgen"))
}

##' Classical susceptible-infected-recovered model
##'
##' A single, unstructured population of hosts.
##'
##' @name sir
##' @aliases SIR SIRS
##' @include getinfo.R lbdp.R
##' @family Genealogy processes
##' @param Beta transmission rate.
##' @param gamma recovery rate.
##' @param psi sampling rate.
##' @param omega immunity waning rate
##' @param S0,I0,R0 initial sizes of susceptible, infected, and recovered populations, respectively.
##' @param time final time
##' @param t0 initial time
##' @return \code{runSIR} and \code{continueSIR} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SIR}.
##' @references
##' \King2024
##'
##' \King2022
##' @example examples/sir.R
NULL

##' @rdname sir
##' @export
runSIR <- function (
  time,  t0 = 0,
  Beta = 2, gamma = 1, psi = 1, omega = 0,
  S0 = 100, I0 = 2, R0 = 0
) {
  params <- c(Beta=Beta,gamma=gamma,psi=psi,omega=omega)
  ivps <- c(S0=S0,I0=I0,R0=R0)
  if (any(ivps < 0))
    pStop(paste(sQuote(names(ivps)),collapse=","),
      " must be nonnegative integers.")
  x <- .Call(P_makeSIR,params,ivps,t0)
  .Call(P_runSIR,x,time) |>
    structure(model="SIR",class=c("gpsim","gpgen"))
}

##' @rdname sir
##' @export
runSIRS <- runSIR

##' @rdname sir
##' @inheritParams simulate
##' @export
continueSIR <- function (
  object, time, Beta = NA, gamma = NA, psi = NA, omega = NA
) {
  params <- c(Beta=Beta,gamma=gamma,psi=psi,omega=omega)
  x <- .Call(P_reviveSIR,object,params)
  .Call(P_runSIR,x,time) |>
    structure(model="SIR",class=c("gpsim","gpgen"))
}

##' @rdname sir
##' @export
runSIRS <- runSIR

##' @rdname sir
##' @export
continueSIRS <- continueSIR

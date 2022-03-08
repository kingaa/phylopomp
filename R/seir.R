##' SEIR model.
##'
##' @name seir
##' @aliases SEIR
##' @include getinfo.R
##' 
##' @family Genealogy processes
##' 
##' @inheritParams sir
##' @param Beta transmission rate for the infectious classes.
##' @param sigma transition rate from exposed class to infectious class.
##' @param gamma recovery rate.
##' @param psi sampling rate.
##' @param delta rate of loss of immunity
##' @param S0 initial size of susceptible population.
##' @param E0 initial size of exposed population.
##' @param I0 initial size of infectious population.
##' @param R0 initial size of recovered population.
##' 
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SEIR}.
##'
##' @example examples/seir.R
##'
NULL

##' @rdname seir
##' @export
runSEIR <- function (
  time, t0 = 0,
  Beta = 5, sigma = 3, gamma = 1,
  psi = 1, delta = 0,
  S0 = 500, E0 = 10, I0 = 10, R0 = 0
) {
  params <- c(Beta=Beta,sigma=sigma,gamma=gamma,psi=psi,delta=delta)
  ivps <- c(S0=S0,E0=E0,I0=I0,R0=R0)
  x <- .Call(P_makeSEIR,params,ivps,t0)
  x <- .Call(P_runSEIR,x,time)
  structure(x,model="SEIR",class="gpsim")
}

##' @rdname seir
##' @export
continueSEIR <- function (
  object, time,
  Beta = NA, sigma = NA, gamma = NA,
  psi = NA, delta = NA
) {
  params <- c(
    Beta=Beta,sigma=sigma,gamma=gamma,psi=psi,delta=delta
  )
  x <- .Call(P_reviveSEIR,object,params)
  .Call(P_runSEIR,x,time)
}

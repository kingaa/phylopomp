##' Linear birth-death-sampling model
##' 
##' The genealogy process induced by a simple linear birth-death process with constant-rate sampling.
##' 
##' @name lbdp
##' @family Genealogy processes
##' @aliases LBDP
##' @param lambda per capita birth rate
##' @param mu per capita death rate
##' @param psi per capita sampling rate
##' @param n0 initial population size
##'
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{LBDP}.
##' 
NULL

##' @rdname lbdp
##' @export
runLBDP <- function (
  time, t0 = 0,
  lambda = 1.3, mu = 1, psi = 1, n0 = 10
) {
  params <- c(lambda=lambda,mu=mu,psi=psi)
  ivps <- c(n0=n0)
  x <- .Call(P_makeLBDP,params,ivps,t0)
  x <- .Call(P_runLBDP,x,time)
  structure(x,model="LBDP",class="gpsim")
}

##' @rdname lbdp
##' @export
continueLBDP <- function (
  object, time,
  lambda = NA, mu = NA, psi = NA
) {
  params <- c(
    lambda=lambda,mu=mu,psi=psi
  )
  x <- .Call(P_reviveLBDP,object,params)
  .Call(P_runLBDP,x,time)
}
##' LBDP
##'
##' Linear birth-death-sampling model.
##'
##' @name lbdp
##' @aliases LBDP
##' @include getinfo.R
##' 
##' @family Genealogy processes
##'
##' @param lambda per capita birth rate
##' @param mu per capita recovery rate.
##' @param psi per capita sampling rate.
##' @param n0 initial population size
##' @param time final time
##' @param t0 initial time
##' 
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{LBDP}.
##'
##' @example examples/lbdp.R
NULL

##' @rdname lbdp
##' @export
runLBDP <- function (
  time,  t0 = 0, 
  lambda = 2, mu = 1, psi = 1,
  n0 = 5
) {
  params <- c(lambda=lambda,mu=mu,psi=psi)
  ics <- c(n0=n0)
  x <- .Call(P_makeLBDP,params,ics,t0)
  x <- .Call(P_runLBDP,x,time)
  structure(x,model="LBDP",class="gpsim")
}

##' @rdname lbdp
##' @inheritParams simulate
##' @export
continueLBDP <- function (
  object, time, lambda = NA, mu = NA, psi = NA
) {
  params <- c(lambda=lambda,mu=mu,psi=psi)
  x <- .Call(P_reviveLBDP,object,params)
  .Call(P_runLBDP,x,time)
}

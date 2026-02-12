##' Linear birth-death-sampling model
##'
##' The genealogy process induced by a simple linear birth-death process with constant-rate sampling.
##'
##' @name lbdp
##' @family Genealogy processes
##' @aliases LBDP
##' @include getinfo.R
##' @param lambda per capita birth rate
##' @param mu per capita death rate
##' @param psi per capita non-destructive sampling rate
##' @param chi per capita destructive sampling rate
##' @param n0 population size at time t0
##' @inheritParams sir
##' @return \code{runLBDP} and \code{continueLBDP} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{LBDP}.
##'
NULL

##' @rdname lbdp
##' @export
runLBDP <- function (
  time, t0 = 0,
  lambda = 2, mu = 1, psi = 1, chi = 0, n0 = 5
) {
  params <- c(lambda=lambda,mu=mu,psi=psi,chi=chi)
  ivps <- c(n0=n0)
  x <- .Call(P_makeLBDP,params,ivps,t0)
  .Call(P_runLBDP,x,time) |>
    structure(model="LBDP",class=c("gpsim","gpgen"))
}

##' @rdname lbdp
##' @export
continueLBDP <- function (
  object, time,
  lambda = NA, mu = NA, psi = NA, chi = NA
) {
  params <- c(
    lambda=lambda,mu=mu,psi=psi,chi=chi
  )
  x <- .Call(P_reviveLBDP,object,params)
  .Call(P_runLBDP,x,time) |>
    structure(model="LBDP",class=c("gpsim","gpgen"))
}
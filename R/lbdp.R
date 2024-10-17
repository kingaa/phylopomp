##' Linear birth-death-sampling model
##'
##' The genealogy process induced by a simple linear birth-death process
##' with constant-rate sampling.
##'
##' @name lbdp
##' @aliases LBDP
##' @include getinfo.R
##' @family Genealogy processes
##' @param lambda per capita birth rate
##' @param mu per capita recovery rate.
##' @param psi per capita sampling rate.
##' @param n0 initial population size
##' @param time final time
##' @param t0 initial time
##' @return \code{runLBDP} and \code{continueLBDP} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{LBDP}.
##' @references
##' \King2024
##'
##' \King2022
##'
##' \Stadler2010
##' @example examples/lbdp.R
NULL

##' @rdname lbdp
##' @export
runLBDP <- function (
  time,  t0 = 0,
  lambda = 2, mu = 1, psi = 1,
  n0 = 5
) {
  n0 <- round(n0)
  if (n0 < 0)
    pStop(sQuote("n0")," must be a nonnegative integer.")
  params <- c(lambda=lambda,mu=mu,psi=psi)
  ivps <- c(n0=n0)
  x <- .Call(P_makeLBDP,params,ivps,t0)
  .Call(P_runLBDP,x,time) |>
    structure(model="LBDP",class=c("gpsim","gpgen"))
}

##' @rdname lbdp
##' @inheritParams simulate
##' @export
continueLBDP <- function (
  object, time, lambda = NA, mu = NA, psi = NA
) {
  params <- c(lambda=lambda,mu=mu,psi=psi)
  x <- .Call(P_reviveLBDP,object,params)
  .Call(P_runLBDP,x,time) |>
    structure(model="LBDP",class=c("gpsim","gpgen"))
}

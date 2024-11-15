##' The classical Moran model
##'
##' The Markov genealogy process induced by the classical Moran process, in which birth/death events occur at a constant rate and the population size remains constant.
##'
##' @name moran
##' @family Genealogy processes
##' @aliases Moran
##' @param mu per capita event rate
##' @param psi per capita sampling rate
##' @param dt time step for state recording
##' @param n population size
##' @inheritParams sir
##' @return \code{runMoran} and \code{continueMoran} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{Moran}.
##'
NULL

##' @rdname moran
##' @export
runMoran <- function (
  time, t0 = 0,
  mu = 1, psi = 1, dt = 0.1, n = 100
) {
  params <- c(mu=mu,psi=psi,dt=dt)
  ivps <- c(n=n)
  x <- .Call(P_makeMoran,params,ivps,t0)
  .Call(P_runMoran,x,time) |>
    structure(model="Moran",class=c("gpsim","gpgen"))
}

##' @rdname moran
##' @export
continueMoran <- function (
  object, time,
  mu = NA, psi = NA, dt = NA
) {
  params <- c(
    mu=mu,psi=psi,dt=dt
  )
  x <- .Call(P_reviveMoran,object,params)
  .Call(P_runMoran,x,time) |>
    structure(model="Moran",class=c("gpsim","gpgen"))
}
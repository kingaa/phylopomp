##' Moran genealogy process
##'
##' Run the simulator.
##'
##' @name moran
##' @aliases Moran
##' @include getinfo.R
##' 
##' @family Genealogy processes
##'
##' @param mu event rate
##' @param psi sampling rate.
##' @param n population size
##' @param stationary logical;
##' if TRUE, the process will be initialized in its stationary distribution.
##' @param time final time
##' @param t0 initial time
##' 
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{"Moran"}.
##'
NULL

##' @rdname moran
##' @export
runMoran <- function (
  time,  t0 = 0, n = 100,
  mu = 1, psi = 1
) {
  params <- c(mu=mu,psi=psi)
  ics <- c(n=n)
  x <- .Call(P_makeMoran,params,ics,t0)
  .Call(P_runMoran,x,time) |>
    structure(model="Moran",class="gpsim")
}

##' @rdname moran
##' @inheritParams simulate
##' @export
continueMoran <- function (
  object, time, mu = NA, psi = NA
) {
  params <- as.numeric(c(mu=mu,psi=psi))
  x <- .Call(P_reviveMoran,object,params)
  .Call(P_runMoran,x,time) |>
    structure(model="Moran",class="gpsim")
}

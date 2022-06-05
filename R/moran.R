##' The classical Moran model
##'
##' The Markov genealogy process induced by the classical Moran process,
##' in which birth/death events occur at a constant rate and the
##' population size remains constant.
##'
##' @name moran
##' @aliases Moran
##' @include getinfo.R
##' 
##' @family Genealogy processes
##'
##' @param mu event rate
##' @param psi sampling rate.
##' @param frac fraction of batch sampling
##' @param n population size
##' @param time final time
##' @param t0 initial time
##' 
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{Moran}.
##'
NULL

##' @rdname moran
##' @export
runMoran <- function (
  time,  t0 = 0, n = 100,
  mu = 1, psi = 1, frac = 0
) {
  params <- c(mu=mu,psi=psi,frac=frac)
  ivps <- c(n=n)
  x <- .Call(P_makeMoran,params,ivps,t0)
  .Call(P_runMoran,x,time) |>
    structure(model="Moran",class="gpsim")
}

##' @rdname moran
##' @inheritParams simulate
##' @export
continueMoran <- function (
  object, time, mu = NA, psi = NA, frac = NA
) {
  params <- as.numeric(c(mu=mu,psi=psi,frac=frac))
  x <- .Call(P_reviveMoran,object,params)
  .Call(P_runMoran,x,time) |>
    structure(model="Moran",class="gpsim")
}

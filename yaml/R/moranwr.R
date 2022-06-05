##' The classical Moran model with reassortment
##'
##' The Markov genealogy process induced by the classical Moran process,
##' in which birth/death events occur at a constant rate and the
##' population size remains constant.
##'
##' @name moranwr
##' @aliases Moranwr
##' @include getinfo.R
##'
##' @family Genealogy processes
##'
##' @param mu event rate
##' @param psi sampling rate
##' @param rhoA reassortment rate for segment A
##' @param rhoB reassortment rate for segment B
##' @param frac fraction of batch sampling
##' @param n population size
##' @param time final time
##' @param t0 initial time
##'
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{Moran}.
##'
NULL

##' @rdname moranwr
##' @export
runMoranwr <- function (
  time,  t0 = 0, n = 100,
  mu = 1, psi = 1,
  rhoA = 0, rhoB = 0, frac = 0
) {
  params <- c(mu=mu,psi=psi,rhoA=rhoA,rhoB=rhoB,frac=frac)
  ivps <- c(n=n)
  x <- .Call(P_makeMoranwr,params,ivps,t0)
  .Call(P_runMoranwr,x,time) |>
    structure(model="Moranwr",class="gpsim")
}

##' @rdname moranwr
##' @inheritParams simulate
##' @export
continueMoranwr <- function (
  object, time, mu = NA, psi = NA,
  rhoA = NA, rhoB = NA, frac = NA
) {
  params <- as.numeric(c(mu=mu,psi=psi,rhoA=rhoA,rhoB=rhoB,frac=frac))
  x <- .Call(P_reviveMoranwr,object,params)
  .Call(P_runMoranwr,x,time) |>
    structure(model="Moranwr",class="gpsim")
}

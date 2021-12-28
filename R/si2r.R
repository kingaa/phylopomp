##' Two-deme model of superspreading
##'
##' Deme 2 consists of "superspreaders" who engender clusters
##' of infection in "superspreading events".
##'
##' @name si2r
##' @aliases SI2R
##' @include getinfo.R
##' 
##' @family Genealogy processes
##' 
##' @inheritParams sir
##' @param Beta transmission rate
##' @param mu mean superspreading-event cluster size
##' @param gamma recovery rate
##' @param delta rate of waning of immunity
##' @param psi1,psi2 sampling rates for demes 1 and 2, respectively
##' @param sigma12,sigma21 movement rates from deme 1 to 2 and 2 to 1, respectively
##' @param S0 initial size of susceptible population
##' @param I0 initial size of I1 population (I2 = 0 at t = 0)
##' @param R0 initial size of recovered population
##'
##' @details
##' Superspreaders (deme 2) behave differently than ordinary infections:
##' transmission events occur at the same rate (\code{Beta}), but at each event,
##' a superspreader infects \eqn{N} individuals, where
##' \deqn{N\sim1+\mathrm{Geometric}(1/\mu).}{N~1+Geometric(1/mu).}
##' Thus, assuming susceptibles are not limiting, the mean number of infections
##' resulting from a superspreading event is \eqn{\mu}{mu}
##' and the variance in this number is \eqn{\mu^2-\mu}{mu^2-mu)}.
##' If susceptibles are limiting,
##' i.e., if the number of susceptibles is not greater than \eqn{N},
##' then all remaining susceptibles are infected.
##' 
##' 
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SI2R}.
##'
##' @example examples/si2r.R
##'
NULL

##' @rdname si2r
##' @export
runSI2R <- function (
  time, t0 = 0,
  Beta = 5, mu = 5, gamma = 1, delta = 0,
  psi1 = 1, psi2 = 0, 
  sigma12 = 1, sigma21 = 3,
  S0 = 500, I0 = 10, R0 = 0
) {
  params <- c(
    Beta=Beta,mu=mu,gamma=gamma,delta=delta,
    psi1=psi1,psi2=psi2,
    sigma12=sigma12,sigma21=sigma21
  )
  ics <- c(S0=S0,I0=I0,R0=R0)
  x <- .Call(P_makeSI2R,params,ics,t0)
  x <- .Call(P_runSI2R,x,time)
  structure(x,model="SI2R",class="gpsim")
}

##' @rdname si2r
##' @inheritParams simulate
##' @export
continueSI2R <- function (
  object, time,
  Beta = NA, mu = NA, gamma = NA, delta = NA,
  psi1 = NA, psi2 = NA,
  sigma21 = NA, sigma12 = NA
) {
  params <- c(
    Beta=Beta,mu=mu,gamma=gamma,delta=delta,
    psi1=psi1,psi2=psi2,
    sigma12=sigma12,sigma21=sigma21
  )
  x <- .Call(P_reviveSI2R,object,params)
  .Call(P_runSI2R,x,time)
}

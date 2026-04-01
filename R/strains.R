##' Three strains compete for a single susceptible pool.
##'
##' The three demes are three distinct pathogen strains that compete for susceptibles.
##'
##' @name strains
##' @family Genealogy processes
##' @aliases Strains
##' @include getinfo.R
##' @param Beta1,Beta2,Beta3 transmission rate for strains 1, 2, 3, respectively
##' @param gamma recovery rate
##' @param psi1,psi2,psi3 sampling rates
##' @inheritParams sir
##' @return \code{runStrains} and \code{continueStrains} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{Strains}.
##'
NULL

##' @rdname strains
##' @export
runStrains <- function (
  time, t0 = 0,
  Beta1 = 5, Beta2 = 5, Beta3 = 5,
  gamma = 1,
  psi1 = 1, psi2 = 0, psi3 = 0,
  S0 = 1, I1_0 = 0.01, I2_0 = 0.01, I3_0 = 0.01, R0 = 0,
  pop=10000
) {
  params <- c(
    Beta1=Beta1,Beta2=Beta2,Beta3=Beta3,
    gamma=gamma,
    psi1=psi1,psi2=psi2,psi3=psi3
  )
  ivps <- c(S0,I1_0,I2_0,I3_0,R0)
  ivps <- structure(
    as.integer(round(pop*ivps/sum(ivps))),
    names=c("S0","I1_0","I2_0","I3_0","R0")
  )
  if (any(ivps < 0))
    pStop(paste(sQuote(names(ivps)),collapse=","),
      " must be nonnegative.")
  x <- .Call(P_makeStrains,params,ivps,t0)
  .Call(P_runStrains,x,time) |>
    structure(model="Strains",class=c("gpsim","gpgen"))
}

##' @rdname strains
##' @export
continueStrains <- function (
  object, time,
  Beta1 = NA, Beta2 = NA, Beta3 = NA,
  gamma = NA, psi1 = NA, psi2 = NA, psi3 = NA
) {
  params <- c(
    Beta1=Beta1,Beta2=Beta2,Beta3=Beta3,
    gamma=gamma,psi1=psi1,psi2=psi2,psi3=psi3
  )
  x <- .Call(P_reviveStrains,object,params)
  .Call(P_runStrains,x,time) |>
    structure(model="Strains",class=c("gpsim","gpgen"))
}

##' Three strains compete for a single susceptible pool.
##'
##' The three demes are three distinct pathogen strains that
##' compete for susceptibles. Sampling is assumed destructive,
##' i.e., sampled individuals are removed from the infectious pool.
##'
##' @name strains
##' @family Genealogy processes
##' @aliases Strains
##' @include getinfo.R
##' @param Beta1 transmission rate for strain 1
##' @param Beta2 transmission rate for strain 2
##' @param Beta3 transmission rate for strain 3
##' @param gamma recovery rate
##' @param chi1 (destructive) sampling rate for strain 1
##' @param chi2 (destructive) sampling rate for strain 2
##' @param chi3 (destructive) sampling rate for strain 3
##' @param pop population size
##' @param S_0 initial susceptible fraction
##' @param I1_0 initial fraction of population infected by strain 1
##' @param I2_0 initial fraction of population infected by strain 2
##' @param I3_0 initial fraction of population infected by strain 2
##' @param R_0 initial fraction of population immune
##' @inheritParams sir
##' @return \code{runStrains} and \code{continueStrains} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{Strains}.
##'
NULL

##' @rdname strains
##' @export
runStrains <- function (
  time, t0 = 0,
  Beta1 = 5, Beta2 = 5, Beta3 = 5, gamma = 1, chi1 = 1, chi2 = 0, chi3 = 0, pop = 1e5, S_0 = 0.8, I1_0 = 0.01, I2_0 = 0.01, I3_0 = 0.01, R_0 = 0.2
) {
  params <- c(Beta1=Beta1,Beta2=Beta2,Beta3=Beta3,gamma=gamma,chi1=chi1,chi2=chi2,chi3=chi3)
  ivps <- c(pop=pop,S_0=S_0,I1_0=I1_0,I2_0=I2_0,I3_0=I3_0,R_0=R_0)
  x <- .Call(P_makeStrains,params,ivps,t0)
  .Call(P_runStrains,x,time) |>
    structure(model="Strains",class=c("gpsim","gpgen"))
}

##' @rdname strains
##' @export
continueStrains <- function (
  object, time,
  Beta1 = NA, Beta2 = NA, Beta3 = NA, gamma = NA, chi1 = NA, chi2 = NA, chi3 = NA
) {
  params <- c(
    Beta1=Beta1,Beta2=Beta2,Beta3=Beta3,gamma=gamma,chi1=chi1,chi2=chi2,chi3=chi3
  )
  x <- .Call(P_reviveStrains,object,params)
  .Call(P_runStrains,x,time) |>
    structure(model="Strains",class=c("gpsim","gpgen"))
}
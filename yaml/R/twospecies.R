##' Two-host infection model with waning, immigration, demography, and spillover. Hosts are culled upon sampling with a given probability.
##'
##' The population is structured by infection progression and host species.
##'
##' @name twospecies
##' @family Genealogy processes
##' @aliases TwoSpecies
##' @param Beta11 transmission rate within species 1
##' @param Beta12 transmission from species 2 to species 1
##' @param Beta21 transmission from species 1 to species 2
##' @param Beta22 transmission rate within species 2
##' @param gamma1 species 1 recovery rate
##' @param gamma2 species 2 recovery rate
##' @param psi1 per capita sampling rate for species 1
##' @param psi2 per capita sampling rate for species 2
##' @param c1 probability that a sampled (positive) host of species 1 is culled
##' @param c2 probability that a sampled (positive) host of species 2 is culled
##' @param omega1 rate of waning of immunity for species 1
##' @param omega2 rate of waning of immunity for species 2
##' @param b1 per capita birth rate for species 1
##' @param b2 per capita birth rate for species 2
##' @param d1 per capita death rate for species 1
##' @param d2 per capita death rate for species 2
##' @param iota1 imported infections for species 1
##' @param iota2 imported infections for species 2
##' @param S1_0 initial size of species 1 susceptible population
##' @param S2_0 initial size of species 2 susceptible population
##' @param I1_0 initial size of species 1 infected population
##' @param I2_0 initial size of species 2 infected population
##' @param R1_0 initial size of species 1 immune population
##' @param R2_0 initial size of species 2 immune population
##' @inheritParams sir
##' @return \code{runTwoSpecies} and \code{continueTwoSpecies} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{TwoSpecies}.
##'
NULL

##' @rdname twospecies
##' @export
runTwoSpecies <- function (
  time, t0 = 0,
  Beta11 = 4, Beta12 = 0, Beta21 = 0, Beta22 = 4, gamma1 = 1, gamma2 = 1, psi1 = 1, psi2 = 0, c1 = 1, c2 = 1, omega1 = 0, omega2 = 0, b1 = 0, b2 = 0, d1 = 0, d2 = 0, iota1 = 0, iota2 = 0, S1_0 = 100, S2_0 = 100, I1_0 = 0, I2_0 = 10, R1_0 = 0, R2_0 = 0
) {
  params <- c(Beta11=Beta11,Beta12=Beta12,Beta21=Beta21,Beta22=Beta22,gamma1=gamma1,gamma2=gamma2,psi1=psi1,psi2=psi2,c1=c1,c2=c2,omega1=omega1,omega2=omega2,b1=b1,b2=b2,d1=d1,d2=d2,iota1=iota1,iota2=iota2)
  ivps <- c(S1_0=S1_0,S2_0=S2_0,I1_0=I1_0,I2_0=I2_0,R1_0=R1_0,R2_0=R2_0)
  x <- .Call(P_makeTwoSpecies,params,ivps,t0)
  .Call(P_runTwoSpecies,x,time) |>
    structure(model="TwoSpecies",class=c("gpsim","gpgen"))
}

##' @rdname twospecies
##' @export
continueTwoSpecies <- function (
  object, time,
  Beta11 = NA, Beta12 = NA, Beta21 = NA, Beta22 = NA, gamma1 = NA, gamma2 = NA, psi1 = NA, psi2 = NA, c1 = NA, c2 = NA, omega1 = NA, omega2 = NA, b1 = NA, b2 = NA, d1 = NA, d2 = NA, iota1 = NA, iota2 = NA
) {
  params <- c(
    Beta11=Beta11,Beta12=Beta12,Beta21=Beta21,Beta22=Beta22,gamma1=gamma1,gamma2=gamma2,psi1=psi1,psi2=psi2,c1=c1,c2=c2,omega1=omega1,omega2=omega2,b1=b1,b2=b2,d1=d1,d2=d2,iota1=iota1,iota2=iota2
  )
  x <- .Call(P_reviveTwoSpecies,object,params)
  .Call(P_runTwoSpecies,x,time) |>
    structure(model="TwoSpecies",class=c("gpsim","gpgen"))
}
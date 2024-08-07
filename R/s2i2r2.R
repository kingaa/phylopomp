##' Two-host infection model with waning, immigration, and demography.
##'
##' The population is structured by infection progression and host species.
##'
##' @name s2i2r2
##' @family Genealogy processes
##' @aliases S2I2R2
##' @param Beta11,Beta22 transmission rates within species 1 and 2, respectively
##' @param Beta12 transmission from species 2 to species 1
##' @param gamma1,gamma2 recovery rates for species 1 and 2, respectively
##' @param psi1,psi2 per capita sampling rates
##' @param omega1,omega2 rates of waning of immunity
##' @param b1,b2 per capita birth rates
##' @param d1,d2 per capita death rates
##' @param iota1,iota2 infection importation rates
##' @param S1_0,S2_0 initial sizes of susceptible populations
##' @param I1_0,I2_0 initial sizes of infected populations
##' @param R1_0,R2_0 initial sizes of immune populations
##' @inheritParams sir
##' @return \code{runS2I2R2} and \code{continueS2I2R2} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{S2I2R2}.
##'
NULL

##' @rdname s2i2r2
##' @export
runS2I2R2 <- function (
  time, t0 = 0,
  Beta11 = 4, Beta12 = 0, Beta22 = 4,
  gamma1 = 1, gamma2 = 1,
  psi1 = 1, psi2 = 0,
  omega1 = 0, omega2 = 0,
  b1 = 0, b2 = 0,
  d1 = 0, d2 = 0,
  iota1 = 0, iota2 = 0,
  S1_0 = 100, S2_0 = 100,
  I1_0 = 0, I2_0 = 10,
  R1_0 = 0, R2_0 = 0
) {
  params <- c(
    Beta11=Beta11,Beta12=Beta12,Beta22=Beta22,
    gamma1=gamma1,gamma2=gamma2,
    psi1=psi1,psi2=psi2,
    omega1=omega1,omega2=omega2,
    b1=b1,b2=b2,
    d1=d1,d2=d2,
    iota1=iota1,iota2=iota2
  )
  ivps <- c(S1_0=S1_0,S2_0=S2_0,I1_0=I1_0,I2_0=I2_0,R1_0=R1_0,R2_0=R2_0)
  x <- .Call(P_makeS2I2R2,params,ivps,t0)
  .Call(P_runS2I2R2,x,time) |>
    structure(model="S2I2R2",class=c("gpsim","gpgen"))
}

##' @rdname s2i2r2
##' @export
continueS2I2R2 <- function (
  object, time,
  Beta11 = NA, Beta12 = NA, Beta22 = NA,
  gamma1 = NA, gamma2 = NA,
  psi1 = NA, psi2 = NA,
  omega1 = NA, omega2 = NA,
  b1 = NA, b2 = NA,
  d1 = NA, d2 = NA,
  iota1 = NA, iota2 = NA
) {
  params <- c(
    Beta11=Beta11,Beta12=Beta12,Beta22=Beta22,
    gamma1=gamma1,gamma2=gamma2,
    psi1=psi1,psi2=psi2,
    omega1=omega1,omega2=omega2,
    b1=b1,b2=b2,
    d1=d1,d2=d2,
    iota1=iota1,iota2=iota2
  )
  x <- .Call(P_reviveS2I2R2,object,params)
  .Call(P_runS2I2R2,x,time) |>
    structure(model="S2I2R2",class=c("gpsim","gpgen"))
}

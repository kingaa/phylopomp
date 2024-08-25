##' Two-host infection model with waning, immigration, demography, and spillover.
##'
##' The population is structured by infection progression and host species.
##'
##' @name twospecies
##' @family Genealogy processes
##' @aliases TwoSpecies
##' @param Beta11,Beta22 transmission rates within species 1 and species 2, respectively
##' @param Beta12 transmission from species 2 to species 1
##' @param Beta21 transmission from species 1 to species 2
##' @param gamma1,gamma2 recovery rates
##' @param psi1,psi2 per capita sampling rates
##' @param omega1,omega2 rates of waning of immunity
##' @param b1,b2 per capita birth rates
##' @param d1,d2 per capita death rates
##' @param iota1,iota2 imported infection rates
##' @param S1_0,S2_0 initial sizes of the susceptible populations in each species
##' @param I1_0,I2_0 initial sizes of infected populations
##' @param R1_0,R2_0 initial sizes of immune populations
##' @inheritParams sir
##' @return \code{runTwoSpecies} and \code{continueTwoSpecies} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{TwoSpecies}.
##'
NULL

##' @rdname twospecies
##' @export
runTwoSpecies <- function (
  time, t0 = 0,
  Beta11 = 0.5, Beta12 = 0.5, Beta21 = 0.06, Beta22 = 1.5,
  gamma1 = 0.5, gamma2 = 0.5, psi1 = 2, psi2 = 0,
  omega1 = 0, omega2 = 0,
  b1 = 0.2, b2 = 0.5,
  d1 = 0.2, d2 = 0.5,
  iota1 = 0, iota2 = 0.001,
  S1_0 = 1000, S2_0 = 300,
  I1_0 = 0, I2_0 = 0,
  R1_0 = 0, R2_0 = 700
) {
  params <- c(
    Beta11=Beta11,Beta12=Beta12,Beta21=Beta21,Beta22=Beta22,
    gamma1=gamma1,gamma2=gamma2,
    psi1=psi1,psi2=psi2,
    omega1=omega1,omega2=omega2,
    b1=b1,b2=b2,d1=d1,d2=d2,
    iota1=iota1,iota2=iota2
  )
  ivps <- c(
    S1_0=S1_0,S2_0=S2_0,
    I1_0=I1_0,I2_0=I2_0,
    R1_0=R1_0,R2_0=R2_0
  )
  x <- .Call(P_makeTwoSpecies,params,ivps,t0)
  .Call(P_runTwoSpecies,x,time) |>
    structure(model="TwoSpecies",class=c("gpsim","gpgen"))
}

##' @rdname twospecies
##' @export
continueTwoSpecies <- function (
  object, time,
  Beta11 = NA, Beta12 = NA, Beta21 = NA, Beta22 = NA, gamma1 = NA, gamma2 = NA, psi1 = NA, psi2 = NA, omega1 = NA, omega2 = NA, b1 = NA, b2 = NA, d1 = NA, d2 = NA, iota1 = NA, iota2 = NA
) {
  params <- c(
    Beta11=Beta11,Beta12=Beta12,Beta21=Beta21,Beta22=Beta22,gamma1=gamma1,gamma2=gamma2,psi1=psi1,psi2=psi2,omega1=omega1,omega2=omega2,b1=b1,b2=b2,d1=d1,d2=d2,iota1=iota1,iota2=iota2
  )
  x <- .Call(P_reviveTwoSpecies,object,params)
  .Call(P_runTwoSpecies,x,time) |>
    structure(model="TwoSpecies",class=c("gpsim","gpgen"))
}

##' @name twospecies_pomp
##' @rdname twospecies
##' @include seir.R
##' @param x genealogy in \pkg{phylopomp} format.
##' @return
##' \code{twospecies_pomp} returns a \sQuote{pomp} object.
##' @details
##' \code{twospecies_pomp} constructs a \sQuote{pomp} object containing a given set of data and a TwoSpecies model.
##' @importFrom pomp pomp onestep
##' @export
twospecies_pomp <- function (
  x,
  Beta11, Beta12, Beta21, Beta22,
  gamma1, gamma2, psi1, psi2, omega1, omega2,
  b1, b2, d1, d2, iota1, iota2,
  S1_0, S2_0, I1_0, I2_0, R1_0, R2_0
)
{
  x |> gendat() -> gi
  ic <- as.integer(c(S1_0,S2_0,I1_0,I2_0,R1_0,R2_0))
  names(ic) <- c("S1_0","S2_0","I1_0","I2_0","R1_0","R2_0")
  if (any(ic < 0))
    pStop(paste(sQuote(names(ic)),collapse=","),
      " must be nonnegative integers.")
  pomp(
    data=NULL,
    t0=gi$nodetime[1L],
    times=gi$nodetime[-1L],
    params=c(
      Beta11=Beta11,Beta12=Beta12,Beta21=Beta21,Beta22=Beta22,
      gamma1=gamma1,gamma2=gamma2,psi1=psi1,psi2=psi2,
      omega1=omega1,omega2=omega2,
      b1=b1,b2=b2,d1=d1,d2=d2,
      iota1=iota1,iota2=iota2,
      S1_0=S1_0,S2_0=S2_0,I1_0=I1_0,I2_0=I2_0,R1_0=R1_0,R2_0=R2_0
    ),
    userdata=gi,
    nstatevars=12L+gi$nsample,
    rinit="twospecies_rinit",
    rprocess=onestep("twospecies_gill"),
    dmeasure="twospecies_dmeas",
    statenames=c(
      "S1","S2","I1","I2","R1","R2","N1","N2",
      "ll","node","ellE","ellI","color"
    ),
    paramnames=c(
      "Beta11","Beta12","Beta21","Beta22",
      "gamma1","gamma2","psi1","psi2","omega1","omega2",
      "b1","b2","d1","d2","iota1","iota2",
      "S1_0","S2_0","I1_0","I2_0","R1_0","R2_0"
    ),
    PACKAGE="phylopomp"
  )
}

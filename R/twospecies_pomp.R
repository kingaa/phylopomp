##' @name twospecies_pomp
##' @rdname twospecies
##' @include twospecies.R
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
  gamma1, gamma2, psi1, psi2, c1, c2,
  omega1, omega2, b1, b2, d1, d2,
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
      b1=b1,b2=b2,d1=d1,d2=d2,c1=c1,c2=c2,
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
      "b1","b2","d1","d2","c1","c2",
      "S1_0","S2_0","I1_0","I2_0","R1_0","R2_0"
    ),
    PACKAGE="phylopomp"
  )
}

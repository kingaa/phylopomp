##' @name strains_pomp
##' @rdname strains
##' @include strains.R
##' @param x genealogy in \pkg{phylopomp} format (i.e., an object that inherits from \sQuote{gpgen}).
##' @details
##' \code{strains_pomp} constructs a \sQuote{pomp} object containing a given set of data and the Strains model.
##' @return
##' \code{strains_pomp} returns a \sQuote{pomp} object.
##' @importFrom pomp pomp onestep
##' @export
strains_pomp <- function (x, Beta1, Beta2, Beta3, gamma, psi1, psi2, psi3, S0, I1_0, I2_0, I3_0, R0, t0 = 0)
{
  x |> gendat(obscure=FALSE) -> gi
  ic <- as.integer(c(S0,I1_0,I2_0,I3_0,R0))
  names(ic) <- c("S0","I1_0","I2_0","I3_0","R0")
  if (any(ic < 0))
    pStop(paste(sQuote(names(ic)),collapse=","),
      " must be nonnegative integers.")
  pomp(
    data=NULL,
    t0=gi$nodetime[1L],
    times=gi$nodetime[-1L],
    params=c(
      Beta1=Beta1,Beta2=Beta2,Beta3=Beta3,
      gamma=gamma,
      psi1=psi1,psi2=psi2,psi3=psi3,
      ic,N=sum(ic)
    ),
    userdata=gi,
    rinit="strains_rinit",
    rprocess=onestep("strains_gill"),
    dmeasure="strains_dmeas",
    statenames=c(
      "S","I2","I2","I3","R",
      "ll","ell1","ell2","ell3","node"
    ),
    paramnames=c(
      "Beta1","Beta2","Beta3","gamma",
      "psi1","psi2","psi3",
      "S0","I1_0","I2_0","I3_0","R0","N"
    ),
    PACKAGE="phylopomp"
  )
}

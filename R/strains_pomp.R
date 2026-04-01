##' @name strains_pomp
##' @rdname strains
##' @include strains.R
##' @param x genealogy in \pkg{phylopomp} format (i.e., an object that inherits from \sQuote{gpgen}).
##' @param S0,I1_0,I2_0,I3_0,R0 initial conditions; non-negative numbers that specify the relative occupancies of the compartments at the inital time.
##' @param pop host population size
##' @details
##' \code{strains_pomp} constructs a \sQuote{pomp} object containing a given set of data and the Strains model.
##' @return
##' \code{strains_pomp} returns a \sQuote{pomp} object.
##' @importFrom pomp pomp onestep parameter_trans
##' @export
strains_pomp <- function (
  x,
  Beta1, Beta2, Beta3, gamma,
  psi1, psi2, psi3, pop,
  S0, I1_0, I2_0, I3_0, R0
) {
  x |> gendat(obscure=FALSE) -> gi
  ivps <- structure(
    c(S0,I1_0,I2_0,I3_0,R0),
    names=c("S0","I1_0","I2_0","I3_0","R0")
  )
  if (any(ivps < 0))
    pStop(paste(sQuote(names(ivps)),collapse=","),
      " must be nonnegative.")
  pomp(
    data=NULL,
    t0=gi$nodetime[1L],
    times=gi$nodetime[-1L],
    params=c(
      Beta1=Beta1,Beta2=Beta2,Beta3=Beta3,
      gamma=gamma,
      psi1=psi1,psi2=psi2,psi3=psi3,
      ivps,pop=pop
    ),
    userdata=gi,
    rinit="strains_rinit",
    rprocess=onestep("strains_gill"),
    dmeasure="strains_dmeas",
    partrans=parameter_trans(
      log=c(
        "Beta1","Beta2","Beta3","gamma",
        "psi1","psi2","psi3"
      ),
      barycentric=c(names(ivps))
    ),
    statenames=c(
      "S","I1","I2","I3","R",
      "ll","ell1","ell2","ell3","node"
    ),
    paramnames=c(
      "Beta1","Beta2","Beta3","gamma",
      "psi1","psi2","psi3",
      names(ivps),"pop"
    ),
    PACKAGE="phylopomp"
  )
}

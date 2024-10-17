##' @name sir_pomp
##' @rdname sir
##' @include sir.R
##' @param x genealogy in \pkg{phylopomp} format (i.e., an object that inherits from \sQuote{gpgen}).
##' @details
##' \code{sir_pomp} constructs a \sQuote{pomp} object containing a given set of data and a SIR model.
##' @return
##' \code{sir_pomp} and \code{sirs_pomp} return \sQuote{pomp} objects.
##' @importFrom pomp pomp onestep
##' @export
sir_pomp <- function (x, Beta, gamma, psi, omega = 0, S0, I0, R0, t0=0)
{
  x |> gendat() -> gi
  ic <- as.integer(c(S0,I0,R0))
  names(ic) <- c("S0","I0","R0")
  if (any(ic < 0))
    pStop(paste(sQuote(names(ic)),collapse=","),
      " must be nonnegative integers.")
  pomp(
    data=NULL,
    t0=gi$nodetime[1L],
    times=gi$nodetime[-1L],
    params=c(Beta=Beta,gamma=gamma,psi=psi,omega=omega,ic,N=sum(ic)),
    userdata=gi,
    rinit="sirs_rinit",
    rprocess=onestep("sirs_gill"),
    dmeasure="sirs_dmeas",
    statenames=c("S","I","R","ll","ell","node"),
    paramnames=c(
      "Beta","gamma","psi","omega",
      "S0","I0","R0","N"
    ),
    PACKAGE="phylopomp"
  )
}

##' @rdname sir
##' @export
sirs_pomp <- sir_pomp

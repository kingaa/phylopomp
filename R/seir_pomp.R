##' @name seirs_pomp
##' @rdname seir
##' @include lbdp.R sir.R seir.R
##' @param x genealogy in \pkg{phylopomp} format.
##' @param S0,E0,I0,R0 initial conditions;
##' non-negative numbers that specify the relative occupancies of the compartments at the inital time.
##' @param pop host population size
##' @return
##' \code{seirs_pomp} returns a \sQuote{pomp} object.
##' @details
##' \code{seirs_pomp} constructs a \sQuote{pomp} object containing a given set of data and an SEIRS model.
##' @importFrom pomp pomp onestep
##' @export
seirs_pomp <- function (
  x,
  Beta, sigma, gamma, psi, omega = 0,
  S0, E0, I0, R0, pop
)
{
  x |> gendat() -> gi
  ivps <- structure(c(S0,E0,I0,R0),names=c("S0","E0","I0","R0"))
  if (any(ivps < 0))
    pStop(paste(sQuote(names(ivps)),collapse=","),
      " must be nonnegative.")
  pomp(
    data=NULL,
    t0=gi$nodetime[1L],
    times=gi$nodetime[-1L],
    params=c(
      Beta=Beta,sigma=sigma,gamma=gamma,psi=psi,omega=omega,
      ivps,pop=pop
    ),
    userdata=gi,
    nstatevars=8L + gi$nsample,
    rinit="seirs_rinit",
    rprocess=onestep("seirs_gill"),
    dmeasure="seirs_dmeas",
    statenames=c(
      "S","E","I","R","ll",
      "node","ellE","ellI","color"
    ),
    paramnames=c(
      "Beta","sigma","gamma","psi","omega",
      "S0","E0","I0","R0","pop"
    ),
    PACKAGE="phylopomp"
  )
}

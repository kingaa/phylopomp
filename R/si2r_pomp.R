##' @name si2rs_pomp
##' @rdname si2r
##' @include si2r.R
##' @param x genealogy in \pkg{phylopomp} format.
##' @return
##' \code{si2rs_pomp} returns a \sQuote{pomp} object.
##' @details
##' \code{si2rs_pomp} constructs a \sQuote{pomp} object containing a given set of data and an SI2RS model.
##' @importFrom pomp pomp onestep
##' @export
si2rs_pomp <- function (
  x,
  Beta, kappa, gamma, omega, chi, etaL, etaH,
  S0, IL0, IH0, R0, pop
)
{
  x |> gendat() -> gi
  ivps <- structure(c(S0,IL0,IH0,R0),names=c("S0","IL0","IH0","R0"))
  if (any(ivps < 0))
    pStop(paste(sQuote(names(ivps)),collapse=","),
      " must be nonnegative.")
  pomp(
    data=NULL,
    t0=gi$nodetime[1L],
    times=gi$nodetime[-1L],
    params=c(
      Beta=Beta,kappa=kappa,gamma=gamma,omega=omega,
      chi=chi,etaL=etaL,etaH=etaH,
      ivps,pop=pop
    ),
    userdata=gi,
    nstatevars=8L + gi$nsample,
    rinit="si2rs_rinit",
    rprocess=onestep("si2rs_gill"),
    dmeasure="si2rs_dmeas",
    statenames=c(
      "S","IL","IH","R","ll",
      "node","ellL","ellH","color"
    ),
    paramnames=c(
      "Beta","kappa","gamma","omega","chi","etaL","etaH",
      "pop","S0","IL0","IH0","R0"
    ),
    PACKAGE="phylopomp"
  )
}

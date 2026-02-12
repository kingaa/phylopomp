##' @rdname lbdp
##' @include lbdp.R
##' @details
##' \code{lbdp_pomp} constructs a \pkg{pomp} object containing a given set of data and a linear birth-death-sampling process.
##' @importFrom pomp pomp onestep covariate_table
##' @inheritParams lbdp_exact
##' @export
lbdp_pomp <- function (x, lambda, mu, psi, chi = 0, n0 = 1)
{
  x |> gendat() -> gi
  n0 <- round(n0)
  if (n0 < 0)
    pStop(sQuote("n0")," must be a nonnegative integer.")
  pomp(
    data=NULL,
    t0=gi$nodetime[1L],
    times=gi$nodetime[-1L],
    params=c(lambda=lambda,mu=mu,psi=psi,chi=chi,n0=n0),
    userdata=gi,
    rinit="lbdp_rinit",
    dmeasure="lbdp_dmeas",
    rprocess=onestep("lbdp_gill"),
    statenames=c("n","ll","ell","node"),
    paramnames=c("lambda","mu","psi","chi","n0"),
    PACKAGE="phylopomp"
  )
}

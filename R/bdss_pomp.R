##' @name bdss_pomp
##' @rdname bdss
##' @include bdss.R
##' @param x genealogy in \pkg{phylopomp} format.
##' @return
##' \code{bdss_pomp} returns a \sQuote{pomp} object.
##' @details
##' \code{bdss_pomp} constructs a \sQuote{pomp} object containing a given set of data and a BDSS model.
##' @importFrom pomp pomp onestep
##' @export
bdss_pomp <- function (
  x,
  lambda_nn, lambda_ns, lambda_sn, lambda_ss,
  mu, chi,
  N0, S0, pop
)
{
  x |> gendat() -> gi
  pars <- list(
    lambda_nn=lambda_nn,lambda_ns=lambda_ns,
    lambda_sn=lambda_sn,lambda_ss=lambda_ss,
    mu=mu,chi=chi,pop=pop,N0=N0,S0=S0
  )
  valid <- vapply(pars,function (p)
    is.numeric(p) && length(p)==1L && is.finite(p) && p>=0,
    logical(1))
  if (any(!valid))
    pStop(paste(sQuote(names(pars)[!valid]),collapse=","),
      " must be finite, nonnegative numeric scalars.")
  ivps <- structure(c(N0,S0),names=c("N0","S0"))
  if (!is.finite(sum(ivps)) || sum(ivps) <= 0)
    pStop(paste(sQuote(names(ivps)),collapse=","),
      " must have a finite positive sum.")
  if (!is.finite(pop/sum(ivps)))
    pStop(sQuote("pop"),"/(",paste(sQuote(names(ivps)),collapse="+"),
      ") must be finite.")
  pomp(
    data=NULL,
    t0=gi$nodetime[1L],
    times=gi$nodetime[-1L],
    params=c(
      lambda_nn=lambda_nn,lambda_ns=lambda_ns,
      lambda_sn=lambda_sn,lambda_ss=lambda_ss,
      mu=mu,chi=chi,
      pop=pop,ivps
    ),
    userdata=gi,
    nstatevars=6L + gi$nsample,
    rinit="bdss_rinit",
    rprocess=onestep("bdss_gill"),
    dmeasure="bdss_dmeas",
    statenames=c(
      "N","S","ll","node","ell1","ell2","color"
    ),
    paramnames=c(
      "lambda_nn","lambda_ns","lambda_sn","lambda_ss",
      "mu","chi","pop","N0","S0"
    ),
    PACKAGE="phylopomp"
  )
}

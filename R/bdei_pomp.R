##' @name bdei_pomp
##' @rdname bdei
##' @include bdei.R
##' @param x genealogy in \pkg{phylopomp} format.
##' @return
##' \code{bdei_pomp} returns a \sQuote{pomp} object.
##' @details
##' \code{bdei_pomp} constructs a \sQuote{pomp} object containing a given set of data and a BDEI model.
##' @importFrom pomp pomp onestep
##' @export
bdei_pomp <- function (
  x,
  sigma, lambda, mu, chi,
  E0, I0, pop
)
{
  x |> gendat() -> gi
  pars <- list(sigma=sigma,lambda=lambda,mu=mu,chi=chi,pop=pop,E0=E0,I0=I0)
  valid <- vapply(pars,function (p)
    is.numeric(p) && length(p)==1L && is.finite(p) && p>=0,
    logical(1))
  if (any(!valid))
    pStop(paste(sQuote(names(pars)[!valid]),collapse=","),
      " must be finite, nonnegative numeric scalars.")
  ivps <- structure(c(E0,I0),names=c("E0","I0"))
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
      sigma=sigma,lambda=lambda,mu=mu,chi=chi,
      pop=pop,ivps
    ),
    userdata=gi,
    nstatevars=6L + gi$nsample,
    rinit="bdei_rinit",
    rprocess=onestep("bdei_gill"),
    dmeasure="bdei_dmeas",
    statenames=c(
      "E","I","ll","node","ellE","ellI","color"
    ),
    paramnames=c(
      "sigma","lambda","mu","chi",
      "pop","E0","I0"
    ),
    PACKAGE="phylopomp"
  )
}

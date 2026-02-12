##' @rdname lbdp
##' @include lbdp.R lbdp_pomp.R
##' @details
##' \code{lbdp_exact} gives the exact log likelihood of a linear birth-death process with (optionally destructive) sampling, conditioned on the population size at time 0.
##' @return \code{lbdp_exact} returns the log likelihood of the genealogy.
##' Note that the time since the most recent sample is informative.
##' @param x genealogy in \pkg{phylopomp} format (i.e., an object that inherits from \sQuote{gpgen}).
##' @references
##' \Stadler2010
##'
##' \King2024
##' @export
lbdp_exact <- function (x, lambda, mu, psi, chi = 0, n0 = 1) {
  x |> gendat() -> gi
  n0 <- as.integer(n0)
  if (n0 < 1) pStop(sQuote("n0")," must be a positive integer.")
  tf <- gi$nodetime[length(gi$nodetime)] ## final time
  t <- gi$nodetime[-length(gi$nodetime)] ## node times
  t0 <- t[1L]                            ## root time
  tbr <- t[gi$nodetype==2L]              ## coalescence times
  ttp <- t[gi$nodetype==1L & gi$saturation==0L] ## tip sample times
  nrt <- sum(gi$nodetype==0L)                   ## number of roots
  nin <- sum(gi$nodetype==1L & gi$saturation==1L) ## number of inline samples
  if (length(ttp) != length(tbr)+nrt)
    pStop("internal inconsistency in ",sQuote("data"),".") #nocov

  d <- sqrt((lambda-mu-psi-chi)^2+4*lambda*(psi+chi)) ## guaranteed to be real
  a <- (lambda+mu+psi+chi)/2/lambda
  b <- d/2/lambda

  G <- function (t) {
    omega <- d*(t-tf)/2
    C <- d*cosh(omega)
    S <- sinh(omega)
    (C+(lambda-mu+psi+chi)*S)/(C+(lambda-mu-psi-chi)*S)
  }

  H <- function (t) {
    omega <- d*(t-tf)/2
    g <- cosh(omega)+(1-a)/b*sinh(omega)
    1/g/g
  }

  lchoose(n0,nrt)+
    lfactorial(nrt)+
    (n0-nrt)*log(G(t0))+
    nrt*log(H(t0))+
    ifelse(nin>0,nin*sum(log(psi)),0)+
    sum(log(2*lambda*H(tbr)))+
    sum(log((psi*G(ttp)+chi)/H(ttp)))
}

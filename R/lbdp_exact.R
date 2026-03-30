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
  t0 <- gi$t0                                   ## root time
  len <- length(gi$nodetime)
  tf <- gi$nodetime[len]                        ## final time
  t <- gi$nodetime[-len]                        ## node times
  tbr <- t[gi$nodetype==2L]                     ## coalescence times
  ttp <- t[gi$nodetype==1L & gi$saturation==0L] ## tip sample times
  nrt <- gi$nroot                               ## number of roots
  nin <- sum(gi$nodetype==1L & gi$saturation==1L) ## number of inline samples
  if (length(ttp) != length(tbr)+nrt)
    pStop("internal inconsistency in ",sQuote("data"),".") #nocov

  a <- lambda-mu+psi+chi
  b <- lambda-mu-psi-chi
  d <- sqrt(b*b+4*lambda*(psi+chi)) ## guaranteed to be real

  G <- function (t) {
    omega <- d*(t-tf)/2
    C <- d*cosh(omega)
    S <- sinh(omega)
    (C+a*S)/(C+b*S)
  }

  H <- function (t) {
    omega <- d*(t-tf)/2
    g <- cosh(omega)+b/d*sinh(omega)
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

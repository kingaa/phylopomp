##' @rdname lbdp
##' @include lbdp.R lbdp_pomp.R
##' @details
##' \code{lbdp_exact} gives the exact log likelihood of a linear birth-death process, conditioned on the population size at time 0.
##' @return \code{lbdp_exact} returns the log likelihood of the genealogy.
##' Note that the time since the most recent sample is informative.
##' @param x genealogy in \pkg{phylopomp} format (i.e., an object that inherits from \sQuote{gpgen}).
##' @references
##' \Stadler2010
##'
##' \King2024
##' @export
lbdp_exact <- function (x, lambda, mu, psi, n0 = 1) {
  x |> gendat() -> gi
  n0 <- as.integer(n0)
  if (n0 < 1) pStop(sQuote("n0")," must be a positive integer.")
  tf <- gi$nodetime[length(gi$nodetime)] ## final time
  t <- gi$nodetime[-length(gi$nodetime)] ## node times
  x0 <- t[1L]                            ## root time
  x <- t[gi$nodetype==2L]                ## coalescence times
  y <- t[gi$nodetype==1L & gi$saturation==0L] ## tip sample times
  l0 <- sum(gi$nodetype==0L)                  ## number of roots
  k <- sum(gi$nodetype==1L & gi$saturation==1L) ## number of inline samples
  if (length(y) != length(x)+l0)
    pStop("internal inconsistency in ",sQuote("data"),".") #nocov

  d <- sqrt((lambda-mu-psi)^2+4*lambda*psi) ## guaranteed to be real
  a <- (lambda+mu+psi)/2/lambda
  b <- d/2/lambda

  G <- function (t) {
    omega <- d*(t-tf)/2
    C <- d*cosh(omega)
    S <- sinh(omega)
    (C+(lambda-mu+psi)*S)/(C+(lambda-mu-psi)*S)
  }

  H <- function (t) {
    omega <- d*(t-tf)/2
    g <- cosh(omega)+(1-a)/b*sinh(omega)
    1/g/g
  }

  lchoose(n0,l0)+
    lfactorial(l0)+
    (n0-l0)*log(G(x0))+
    l0*log(H(x0))+
    k*log(psi)+
    sum(log(2*lambda*H(x)))+
    sum(log(psi*G(y)/H(y)))
}

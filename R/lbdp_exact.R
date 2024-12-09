##' @rdname lbdp
##' @include lbdp.R
##' @details
##' \code{lbdp_exact} gives the exact log likelihood of a linear birth-death process, conditioned on \eqn{n_0 = 0}{n0=0} (Stadler, 2010, Thm 3.5).
##' The derivation is also given in comments in the code.
##' @return \code{lbdp_exact} returns the log likelihood of the genealogy.
##' Note that the time since the most recent sample is informative.
##' @param x genealogy in \pkg{phylopomp} format (i.e., an object that inherits from \sQuote{gpgen}).
##' @references
##' \Stadler2010
##' @export
lbdp_exact <- function (x, lambda, mu, psi, n0 = 1) {
  ## Theorem 3.5 in Stadler (2010) with rho=0

  ## Here, we reverse the direction of time.
  ## Suppose we maintain surveillance at rate psi > 0 from time 0 onward
  ## (i.e., psi = 0 for t < 0, psi = const > 0 for t > 0).
  ## Suppose we have a linear birth-death-sampling process, n(t), with
  ## constant rates; assume that n(t_or) = 1.
  ##
  ## Let p0(t) = probability that an individual alive at time t is ancestral to no samples.
  ## Then dp0/dt = mu - (lambda+mu+psi) p0 + lambda p0^2 and p0(0) = 1.
  ## Let f(z) = (1-z)/(1+z).  Note f(f(z))=z.
  ## Changing variables and time according to:
  ##    p0 = a + b f(z), s = d t,
  ## where a, b, d are as below,
  ## we obtain dz/ds = z.
  ## Moreover z(0) = z0 = f((p(0)-a)/b) = f((1-a)/b).
  ## Consequently, p0(t) = a + b f(z0 exp(d t)).
  ##
  ## Let g(t) = the probability density of a genealogy rooted at time t2 > t > t1,
  ## where t1, t2 are consecutive node times, satisfies
  ## dg/dt = -(lambda+mu+psi) g + 2 lambda p0 g, where p0 is as above.
  ## Making the change of variables g = q(t) h, where
  ##    Q(t) = exp(d t) / (1 + z0 exp(d t))^2,
  ## gives dh/dt = 0.
  ## It follows that g(t2)/g(t1) = Q(t2)/Q(t1).
  ## One can combine these formulae into one for the likelihood of
  ## the full genealogy rooted at time t_or. To do so, let
  ## x0 = t_or and xi = the time of the i-th branch point of the genealogy.
  ## Let yj = the time of the jth live sample
  ## (a live sample is one that is ancestral to no other sample).
  ## Let L(G) be the likelihood of the given genealogy.
  ## Then
  ##    log L(G) = (m-1) log(lambda) + (k+m) log(psi)
  ##                  + sum(log(Q(xi),i=0,...,m-1)
  ##                  + sum(log(p0(yj)/Q(yj)),j=1,...,m).
  ## Here, m = number of live samples, k = number of dead samples.
  ##
  ## Note that the Q here is the reciprocal of the q in Stadler (2010).
  x |>
    lineages(prune=TRUE,obscure=TRUE) |>
    encode_data() -> data
  n0 <- as.integer(n0)
  if (n0 < 1) pStop(sQuote("n0")," must be a positive integer.")
  tf <- data$time[nrow(data)]
  x0 <- tf-data$time[1L]           ## root time
  x <- tf-data$time[data$code==1]  ## coalescence times
  y <- tf-data$time[data$code==-1] ## live samples
  n <- data$lineages[1L]           ## number of roots
  k <- sum(data$code==0)           ## number of dead samples
  m <- sum(data$code==-1)          ## number of live samples

  if (m - n != length(x))
    pStop("internal inconsistency in ",sQuote("data"),".") #nocov

  ## A simple fractional linear transformation (1-z)/(1+z),
  ## defined on the whole of the Riemann sphere.
  f <- function (z) {
    if_else(
      is.infinite(z),
      -1,
      if_else(
        z==-1,
        Inf,
        (1-z)/(1+z)
      )
    )
  }
  d <- sqrt((lambda-mu-psi)^2+4*lambda*psi) ## guaranteed to be real
  a <- (lambda+mu+psi)/2/lambda
  b <- d/2/lambda
  z0 <- f((1-a)/b)

  p0 <- function (t) {
    a+b*f(z0*exp(d*t))
  }

  Q <- function (t) {
    e <- exp(d*t)
    g <- 1+z0*e
    e/g/g
  }

  lchoose(n0,n)+
    lfactorial(n)+
    (n0-n)*log(p0(x0))+
    (m-n)*log(2*lambda)+
    (k+m)*log(psi)+
    n*log(Q(x0))+
    sum(log(Q(x)))+
    sum(log(p0(y)/Q(y)))
}

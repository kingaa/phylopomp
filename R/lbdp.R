##' Linear birth-death-sampling model
##'
##' The genealogy process induced by a simple linear birth-death process
##' with constant-rate sampling.
##'
##' @name lbdp
##' @aliases LBDP
##' @include getinfo.R
##' 
##' @family Genealogy processes
##'
##' @param lambda per capita birth rate
##' @param mu per capita recovery rate.
##' @param psi per capita sampling rate.
##' @param n0 initial population size
##' @param time final time
##' @param t0 initial time
##' 
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{LBDP}.
##'
##' @example examples/lbdp.R
NULL

##' @rdname lbdp
##' @export
runLBDP <- function (
  time,  t0 = 0, 
  lambda = 2, mu = 1, psi = 1,
  n0 = 5
) {
  params <- c(lambda=lambda,mu=mu,psi=psi)
  ivps <- c(n0=n0)
  x <- .Call(P_makeLBDP,params,ivps,t0)
  x <- .Call(P_runLBDP,x,time)
  structure(x,model="LBDP",class="gpsim")
}

##' @rdname lbdp
##' @inheritParams simulate
##' @export
continueLBDP <- function (
  object, time, lambda = NA, mu = NA, psi = NA
) {
  params <- c(lambda=lambda,mu=mu,psi=psi)
  x <- .Call(P_reviveLBDP,object,params)
  .Call(P_runLBDP,x,time)
}

##' @name lbdp_exact
##' @rdname lbdp
##' @details
##' \code{lbdp_exact} gives the exact likelihood of a linear birth-death process, conditioned on \eqn{n_0 = 0}{n0=0} (Stadler, 2010, Thm 3.5).
##' The derivation is also given in comments in the code.
##'
##' The \code{data} argument should in the format returned by \code{\link{newick2df}}.
##'
##' @return \code{lbdp_exact} returns the log likelihood of the genealogy.
##' Note that the time since the most recent sample is informative.
##'
##' @param data data frame containing the genealogy event times and event codes.
##' 
##' @references
##'
##' \Stadler2010
##' 
##' @export
lbdp_exact <- function (data, lambda, mu, psi) {
  ## Theorem 3.5 in Stadler (2010) with rho=0

  ## Here, we reverse the direction of time.
  ## Suppose we maintain surveillance at rate psi > 0 from time 0 onward
  ## (i.e., psi = 0 tor t < 0, psi = const > 0 for t > 0).
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
  ##    Q(t) = exp(d t) / (1 + z0 exp(d t))^2
  ## Gives dh/dt = 0.
  ## It follows that g(t2)/g(t1) = Q(t2)/Q(t1).
  ## One can combine these formulae into one for the likelihood of the full genealogy
  ## rooted at time t_or. To do so, let x0 = t_or and xi = the time of the i-th
  ## branch point of the genealogy.  Let yj = the time of the jth live sample
  ## (a live sample is one that is ancestral to no other sample).  Let L(G) be the
  ## likelihood of the given genealogy.  Then
  ##    log L(G) = (m-1) log(lambda) + (k+m) log(psi)
  ##                  + sum(log(Q(xi),i=0,...,m-1)
  ##                  + sum(log(p0(yj)/Q(yj)),j=1,...,m).
  ## Here, m = number of live samples, k = number of dead samples.
  ##
  ## Note that the Q here is the reciprocal of the q in Stadler (2010).

##' @importFrom utils tail
  tf <- tail(data$time,1L)
  x <- tf-data$time[data$code==1 | data$code==2] ## coalescence times (including root time)
  y <- tf-data$time[data$code==-1]                ## live samples
  k <- sum(data$code==0)  ## k: no. of dead samples
  m <- sum(data$code==-1) ## m: no. of live samples
  ##  if (m - length(x) != data$lineages[1L]-1L)
  if (m - length(x) != 0L)
    stop("internal inconsistency in ",sQuote("data"),".",call.=FALSE)

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

  (m-1)*log(2*lambda)+(k+m)*log(psi)+
    sum(log(Q(x)))+
    sum(log(p0(y)/Q(y)))
}

##' @name lbdp_pomp
##' @rdname lbdp
##'
##' @details
##' \code{lbdp_pomp} constructs a \pkg{pomp} object containing a given set of data and a linear birth-death-sampling process.
##'
##' It is assumed that \code{data} is in the format returned by \code{\link{newick2df}}.
##'
##' @importFrom pomp pomp onestep covariate_table
##' @inheritParams lbdp_exact
##'
##' @export

lbdp_pomp <- function (data, lambda, mu, psi, n0 = 1, t0 = 0)
{
  data <- as.data.frame(data)
  data["time"] |>
    pomp(
      times="time",t0=t0,
      params=c(lambda=lambda,mu=mu,psi=psi,n0=n0),
      rinit="lbdp_rinit",
      dmeasure="lbdp_dmeas",
      paramnames=c("lambda","mu","n0","psi"),
      accumvars=c("ll"),
      statenames=c("n","ll"),
      PACKAGE="phylopomp",
      covar=covariate_table(
        data,
        times="time",
        order="constant"
      ),
      rprocess=onestep("lbdp_gill")
    )
}

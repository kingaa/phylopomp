##' @rdname lbdp
##' @include lbdp.R lbdp_pomp.R
##' @details
##' \code{lbdp_exact} gives the exact log likelihood of a linear birth-death process with sampling (and optional destructive sampling), conditioned on the population size at time 0.
##' Supports the full BDD(r) model: \code{r=0} is non-destructive sampling, \code{r=1} is fully destructive, and \code{0 < r < 1} mixes both.
##' @return \code{lbdp_exact} returns the log likelihood of the genealogy.
##' Note that the time since the most recent sample is informative.
##' @param x genealogy in \pkg{phylopomp} format (i.e., an object that inherits from \sQuote{gpgen}).
##' @param r probability that a sampled lineage is removed (must be between 0 and 1).
##' @references
##' \Stadler2010
##'
##' \King2024
##' @export
lbdp_exact <- function (x, lambda, mu, psi, r = 0, n0 = 1) {
  if (!is.numeric(r) || length(r) != 1L || !is.finite(r) || r < 0 || r > 1)
    pStop(sQuote("r")," must be between 0 and 1.")
  x |>
    lineages(prune=TRUE,obscure=TRUE) |>
    encode_data() -> data
  n0 <- as.integer(n0)
  if (n0 < 1) pStop(sQuote("n0")," must be a positive integer.")
  tf <- data$time[nrow(data)]
  x0 <- data$time[1L]              ## root time
  x <- data$time[data$code==1L]    ## coalescence times
  l0 <- data$lineages[1L]          ## number of roots
  ## Sample events: code 0 = inline, code -1 = tip; saturation 1 = non-destructive, 0 = destructive
  sample_idx <- data$code %in% c(0L, -1L)
  y_all <- data$time[sample_idx]
  sat_all <- data$saturation[sample_idx]
  y_nd <- y_all[sat_all == 1L]    ## non-destructive (lineage continues)
  y_d <- y_all[sat_all == 0L]     ## destructive (lineage removed)
  if (length(y_all) != length(x) + l0)
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

  ## Root and coalescence terms (unchanged by r)
  out <- lchoose(n0,l0) +
    lfactorial(l0) +
    (n0-l0)*log(G(x0)) +
    l0*log(H(x0)) +
    sum(log(2*lambda*H(x)))
  ## Sampling: non-destructive contributes psi*(1-r)*G(t)/H(t); destructive contributes psi*r
  if (length(y_nd) > 0L)
    out <- out + sum(log(psi*(1-r)*G(y_nd)/H(y_nd)))
  if (length(y_d) > 0L)
    out <- out + sum(log(psi*r))
  out
}

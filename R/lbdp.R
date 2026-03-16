##' Linear birth-death-sampling model
##'
##' The genealogy process induced by a simple linear birth-death process with constant-rate sampling.
##'
##' @name lbdp
##' @family Genealogy processes
##' @aliases LBDP
##' @include getinfo.R
##' @param lambda per capita birth rate
##' @param mu per capita death rate
##' @param psi per capita non-destructive sampling rate
##' @param chi per capita destructive sampling rate
##' @param n0 population size at time t0
##' @param min_tips optional; when set with \code{max_tips}, reject trees until tip count is in [min_tips, max_tips]
##' @param max_tips optional; when set with \code{min_tips}, reject trees until tip count is in [min_tips, max_tips]
##' @inheritParams sir
##' @return \code{runLBDP} and \code{continueLBDP} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{LBDP}.
##' @references
##' \King2024
##'
##' \King2022
##'
##' \Stadler2010
##' @example examples/lbdp.R
NULL

##' @rdname lbdp
##' @export
runLBDP <- function (
  time, t0 = 0,
  lambda = 2, mu = 1, psi = 1, chi = 0, n0 = 5,
  min_tips = NULL, max_tips = NULL
) {
  n0 <- round(n0)
  if (n0 < 0)
    pStop(sQuote("n0")," must be a nonnegative integer.")
  use_range <- !is.null(min_tips) && !is.null(max_tips)
  if (use_range && (min_tips < 1L || max_tips < min_tips))
    pStop("invalid tip range: min_tips and max_tips must satisfy 1 <= min_tips <= max_tips.")

  repeat {
    params <- c(lambda=lambda,mu=mu,psi=psi,chi=chi)
    ivps <- c(n0=n0)
    x <- .Call(P_makeLBDP,params,ivps,t0)
    out <- .Call(P_runLBDP,x,time) |>
      structure(model="LBDP",class=c("gpsim","gpgen"))
    if (!use_range) return(out)
    n <- getInfo(out,nsample=TRUE)$nsample
    if (n >= min_tips && n <= max_tips) return(out)
    ## optionally perturb time for next attempt (improves yield)
    time <- time * runif(1,0.9,1.1)
  }
}

##' @rdname lbdp
##' @inheritParams simulate
##' @export
continueLBDP <- function (
  object, time,
  lambda = NA, mu = NA, psi = NA, chi = NA
) {
  params <- c(
    lambda=lambda,mu=mu,psi=psi,chi=chi
  )
  x <- .Call(P_reviveLBDP,object,params)
  .Call(P_runLBDP,x,time) |>
    structure(model="LBDP",class=c("gpsim","gpgen"))
}

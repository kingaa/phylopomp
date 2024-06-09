##' Classical susceptible-infected-recovered model
##'
##' A single, unstructured population of hosts.
##'
##' @name sir
##' @aliases SIR SIRS
##' @include getinfo.R
##' @family Genealogy processes
##' @param Beta transmission rate.
##' @param gamma recovery rate.
##' @param psi sampling rate.
##' @param omega immunity waning rate
##' @param S0,I0,R0 initial sizes of susceptible, infected, and recovered populations, respectively.
##' @param time final time
##' @param t0 initial time
##' @return \code{runSIR} and \code{continueSIR} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SIR}.
##' @references
##' \King2024
##'
##' \King2022
##' @example examples/sir.R
NULL

##' @rdname sir
##' @export
runSIR <- function (
  time,  t0 = 0,
  Beta = 2, gamma = 1, psi = 1, omega = 0,
  S0 = 100, I0 = 2, R0 = 0
) {
  params <- c(Beta=Beta,gamma=gamma,psi=psi,omega=omega)
  ivps <- c(S0=S0,I0=I0,R0=R0)
  if (any(ivps < 0))
    pStop(paste(sQuote(names(ivps)),collapse=","),
      " must be nonnegative integers.")
  x <- .Call(P_makeSIR,params,ivps,t0)
  .Call(P_runSIR,x,time) |>
    structure(model="SIR",class=c("gpsim","gpgen"))
}

##' @rdname sir
##' @export
runSIRS <- runSIR

##' @rdname sir
##' @inheritParams simulate
##' @export
continueSIR <- function (
  object, time, Beta = NA, gamma = NA, psi = NA, omega = NA
) {
  params <- c(Beta=Beta,gamma=gamma,psi=psi,omega=omega)
  x <- .Call(P_reviveSIR,object,params)
  .Call(P_runSIR,x,time) |>
    structure(model="SIR",class=c("gpsim","gpgen"))
}

##' @name sir_pomp
##' @rdname sir
##' @include lbdp.R
##' @param x genealogy in \pkg{phylopomp} format (i.e., an object that inherits from \sQuote{gpgen}).
##' @details
##' \code{sir_pomp} constructs a \sQuote{pomp} object containing a given set of data and a SIR model.
##' @return
##' \code{sir_pomp} and \code{sirs_pomp} return \sQuote{pomp} objects.
##' @importFrom pomp pomp onestep
##' @export
sir_pomp <- function (x, Beta, gamma, psi, omega = 0, S0, I0, R0, t0=0)
{
  x |> gendat() -> gi
  ic <- as.integer(c(S0,I0,R0))
  names(ic) <- c("S0","I0","R0")
  if (any(ic < 0))
    pStop(paste(sQuote(names(ic)),collapse=","),
      " must be nonnegative integers.")
  pomp(
    data=NULL,
    t0=gi$nodetime[1L],
    times=gi$nodetime[-1L],
    params=c(Beta=Beta,gamma=gamma,psi=psi,omega=omega,ic,N=sum(ic)),
    userdata=gi,
    rinit="sirs_rinit",
    rprocess=onestep("sirs_gill"),
    dmeasure="sirs_dmeas",
    statenames=c("S","I","R","ll","ell","node"),
    paramnames=c(
      "Beta","gamma","psi","omega",
      "S0","I0","R0","N"
    ),
    PACKAGE="phylopomp"
  )
}

##' @rdname sir
##' @export
runSIRS <- runSIR

##' @rdname sir
##' @export
continueSIRS <- continueSIR

##' @rdname sir
##' @export
sirs_pomp <- sir_pomp

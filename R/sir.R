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
##' @param delta immunity waning rate
##' @param S0 initial size of susceptible population.
##' @param I0 initial size of infected population.
##' @param R0 initial size of recovered population.
##' @param time final time
##' @param t0 initial time
##' @return \code{runSIR} and \code{continueSIR} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SIR}.
##'
##' @example examples/sir.R
NULL

##' @rdname sir
##' @export
runSIR <- function (
  time,  t0 = 0, 
  Beta = 2, gamma = 1, psi = 1, delta = 0,
  S0 = 100, I0 = 2, R0 = 0
) {
  params <- c(Beta=Beta,gamma=gamma,psi=psi,delta=delta)
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
  object, time, Beta = NA, gamma = NA, psi = NA, delta = NA
) {
  params <- c(Beta=Beta,gamma=gamma,psi=psi,delta=delta)
  x <- .Call(P_reviveSIR,object,params)
  .Call(P_runSIR,x,time) |>
    structure(model="SIR",class=c("gpsim","gpgen"))
}

##' @name sir_pomp
##' @rdname sir
##' @include lbdp.R
##' @param data data frame containing the lineage count function
##' @details
##' \code{sir_pomp} constructs a \pkg{pomp} object containing a given set of data and a SIR model.
##' @importFrom pomp pomp onestep covariate_table
##' @export
sir_pomp <- function (data, Beta, gamma, psi, delta = 0, S0, I0, R0, t0=0)
{
  ic <- as.integer(c(S0=S0,I0=I0,R0=R0))
  if (any(ic < 0))
    pStop(paste(sQuote(names(ic)),collapse=","),
      " must be nonnegative integers.")
  names(ic) <- c("S0","I0","R0")
  data <- encode_data(data)
  data["time"] |>
    pomp(
      times="time",t0=t0,
      params=c(Beta=Beta,gamma=gamma,psi=psi,delta=delta,ic,N=sum(ic)),
      covar=covariate_table(
        data,
        times="time",
        order="constant"
      ),
      rinit="sirs_rinit",
      rprocess=onestep("sirs_gill"),
      dmeasure="sirs_dmeas",
      accumvars=c("ll"),
      statenames=c("S","I","R","ll"),
      paramnames=c("Beta","gamma","psi","delta","S0","I0","R0","N"),
      covarnames=c("lineages","code"),
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

##' SIR with sampling simulator.
##'
##' Run the simulator.
##'
##' @name sir
##' @aliases SIR
##' @include getinfo.R
##' 
##' @family Genealogy processes
##'
##' @param Beta transmission rate.
##' @param gamma recovery rate.
##' @param psi sampling rate.
##' @param S0 initial size of susceptible population.
##' @param I0 initial size of infected population.
##' @param R0 initial size of recovered population.
##' @param time final time
##' @param t0 initial time
##' 
##' @return An object of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{"SIR"}.
##'
##' @example examples/sir.R
NULL

##' @rdname sir
##' @export
runSIR <- function (
  Beta = 2, gamma = 1, psi = 1,
  S0 = 100, I0 = 2, R0 = 0,
  t0 = 0, time = 1
) {
  params <- c(Beta=Beta,gamma=gamma,psi=psi)
  ics <- c(S0=S0,I0=I0,R0=R0)
  x <- .Call(P_makeSIR,params,ics,t0)
  x <- .Call(P_runSIR,x,time)
  attr(x,"model") <- "SIR"
  class(x) <- "gpsim"
  x
}

##' @rdname sir
##' @inheritParams continue
##' @export
continueSIR <- function (
  object, time, Beta = NA, gamma = NA, psi = NA
) {
  params <- c(Beta=Beta,gamma=gamma,psi=psi)
  x <- .Call(P_reviveSIR,object,params)
  x <- .Call(P_runSIR,x,time)
  x
}

##' @name sir_pomp
##' @rdname sir
##'
##' @param data data frame containing the genealogy in the format returned by \code{\link{newick2df}}.
##' @details
##' \code{sir_pomp} constructs a \pkg{pomp} object containing a given set of data and a SIR model.
##'
##' @importFrom pomp pomp onestep covariate_table
##'
##' @export
sir_pomp <- function (data, Beta, gamma, psi, S0, I0, R0, t0=0)
{
  S0 <- as.integer(S0)
  I0 <- as.integer(I0)
  R0 <- as.integer(R0)
  if (S0 < 0 || I0 < 0 || R0 < 0)
    stop(sQuote("S0"),", ",sQuote("I0"),", and ",sQuote("R0"),
      " must be nonnegative integers.",.call=FALSE)
  data[,"time"] |>
    pomp(
      times="time",t0=t0,
      params=c(Beta=Beta,gamma=gamma,psi=psi,S0=S0,I0=I0,R0=R0,N=S0+I0+R0),
      rinit="sir_rinit",
      dmeasure="sir_dmeas",
      paramnames=c("Beta","gamma","psi","S0","I0","R0","N"),
      accumvars=c("ll"),
      statenames=c("S","I","R","ll"),
      PACKAGE="phylopomp",
      covar=covariate_table(
        data,
        times="time",
        order="constant"
      ),
      rprocess=onestep("sir_gill")
    )
}

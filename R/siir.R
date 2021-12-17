##' SIIR with sampling simulator.
##'
##' Run the simulator.
##'
##' @name siir
##' @aliases SIIR
##' @include getinfo.R
##' 
##' @family Genealogy processes
##' 
##' @inheritParams sir
##' @param Beta1,Beta2 transmission rates from each of the infectious classes.
##' @param gamma recovery rate.
##' @param psi sampling rate.
##' @param S0 initial size of susceptible population.
##' @param I1_0 initial size of I2 population.
##' @param I2_0 initial size of I2 population.
##' @param R0 initial size of recovered population.
##' 
##' @return A \code{tibble} with \code{state} attribute.
##'
##' @example examples/siir.R
##'
##' @importFrom dplyr bind_rows filter
##' @importFrom tibble as_tibble
##' @importFrom utils globalVariables
##'
##' @rdname siir
##' @export
playSIIR <- function (
  Beta1 = 2, Beta2 = 2, gamma = 1, psi = 1,
  S0 = 100, I1_0 = 1, I2_0 = 1, R0 = 0,
  t0 = 0, times
) {
  params <- c(Beta1=Beta1,Beta2=Beta2,gamma=gamma,psi=psi)
  ics <- c(S0=S0,I1_0=I1_0,I2_0=I2_0,R0=R0)
  x <- .Call(P_makeSIIR,params,ics,t0)
  x <- .Call(P_runSIIR,x,times)
  state <- x$state
  x$state <- NULL
  x |> as_tibble() |> filter(!is.na(count)) -> x
  attr(x,"state") <- state
  attr(x,"model") <- "SIIR"
  if (!inherits(x,"gpsim")) class(x) <- c("gpsim",class(x))
  x
}

continueSIIR <- function (
  state, times, Beta1 = NA, Beta2 = NA, gamma = NA, psi = NA
) {
  params <- c(Beta1=Beta1,Beta2=Beta2,gamma=gamma,psi=psi)
  x <- .Call(P_reviveSIIR,state,params)
  x <- .Call(P_runSIIR,x,times)
  x
}

utils::globalVariables("count")

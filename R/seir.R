##' Classical susceptible-exposed-infected-recovered model
##'
##' The population is structured by infection progression.
##'
##' @name seir
##' @family Genealogy processes
##' @aliases SEIR
##' @param Beta transmission rate
##' @param sigma progression rate
##' @param gamma recovery rate
##' @param psi per capita sampling rate
##' @param omega rate of waning of immunity
##' @param S0,E0,I0,R0 initial sizes of S, E, I, R compartments, respectively.
##' @inheritParams sir
##' @return \code{runSEIR} and \code{continueSEIR} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SEIR}.
##' @references
##' \King2024
##' @example examples/seir.R
NULL

##' @rdname seir
##' @export
runSEIR <- function (
  time, t0 = 0,
  Beta = 4, sigma = 1, gamma = 1, psi = 1, omega = 0, S0 = 100, E0 = 5, I0 = 5, R0 = 0
) {
  params <- c(Beta=Beta,sigma=sigma,gamma=gamma,psi=psi,omega=omega)
  ivps <- c(S0=S0,E0=E0,I0=I0,R0=R0)
  if (any(ivps < 0))
    pStop(paste(sQuote(names(ivps)),collapse=","),
      " must be nonnegative integers.")
  x <- .Call(P_makeSEIR,params,ivps,t0)
  .Call(P_runSEIR,x,time) |>
    structure(model="SEIR",class=c("gpsim","gpgen"))
}

##' @rdname seir
##' @export
runSEIRS <- runSEIR

##' @rdname seir
##' @inheritParams simulate
##' @export
continueSEIR <- function (
  object, time,
  Beta = NA, sigma = NA, gamma = NA, psi = NA, omega = NA
) {
  params <- c(
    Beta=Beta,sigma=sigma,gamma=gamma,psi=psi,omega=omega
  )
  x <- .Call(P_reviveSEIR,object,params)
  .Call(P_runSEIR,x,time) |>
    structure(model="SEIR",class=c("gpsim","gpgen"))
}

##' @rdname seir
##' @export
continueSEIRS <- continueSEIR

##' Classical susceptible-exposed-infected-recovered model
##'
##' The population is structured by infection progression.
##'
##' @name seir
##' @aliases SEIR
##' @family Genealogy processes
##' @include sir.R
##' @param Beta transmission rate
##' @param sigma progression rate
##' @param gamma recovery rate
##' @param psi per capita sampling rate
##' @param omega rate of waning of immunity
##' @param S0,E0,I0,R0 initial sizes of S, E, I, R compartments, respectively.
##' @inheritParams runSIR
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

##' @name seirs_pomp
##' @rdname seir
##' @include lbdp.R sir.R
##' @param x genealogy in \pkg{phylopomp} format.
##' @return
##' \code{seirs_pomp} returns a \sQuote{pomp} object.
##' @details
##' \code{seirs_pomp} constructs a \sQuote{pomp} object containing a given set of data and an SEIRS model.
##' @importFrom pomp pomp onestep
##' @export
seirs_pomp <- function (
  x,
  Beta, sigma, gamma, psi, omega = 0,
  S0, E0, I0, R0
)
{
  x |> gendat() -> gi
  ic <- as.integer(c(S0,E0,I0,R0))
  names(ic) <- c("S0","E0","I0","R0")
  if (any(ic < 0))
    pStop(paste(sQuote(names(ic)),collapse=","),
      " must be nonnegative integers.")
  pomp(
    data=NULL,
    t0=gi$nodetime[1L],
    times=gi$nodetime[-1L],
    params=c(
      Beta=Beta,sigma=sigma,gamma=gamma,psi=psi,omega=omega,
      ic,N=sum(ic)
    ),
    userdata=gi,
    nstatevars=8L + gi$nsample,
    rinit="seirs_rinit",
    rprocess=onestep("seirs_gill"),
    dmeasure="seirs_dmeas",
    statenames=c(
      "S","E","I","R","ll",
      "node","ellE","ellI","color"
    ),
    paramnames=c(
      "Beta","sigma","gamma","psi","omega",
      "S0","E0","I0","R0","N"
    ),
    PACKAGE="phylopomp"
  )
}

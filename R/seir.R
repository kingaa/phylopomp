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
##' @param delta rate of waning of immunity
##' @param S0 initial size of susceptible population
##' @param E0 initial size of exposed population
##' @param I0 initial size of infected population
##' @param R0 initial size of immune population
##' @inheritParams runSIR
##' @return \code{runSEIR} and \code{continueSEIR} return objects of class \sQuote{gpsim} with \sQuote{model} attribute \dQuote{SEIR}.
##'
##' @example examples/seir.R
##'
NULL

##' @rdname seir
##' @export
runSEIR <- function (
  time, t0 = 0,
  Beta = 4, sigma = 1, gamma = 1, psi = 1, delta = 0, S0 = 100, E0 = 5, I0 = 5, R0 = 0
) {
  params <- c(Beta=Beta,sigma=sigma,gamma=gamma,psi=psi,delta=delta)
  ivps <- c(S0=S0,E0=E0,I0=I0,R0=R0)
  if (any(ivps < 0))
    pStop(paste(sQuote(names(ivps)),collapse=","),
      " must be nonnegative integers.")
  x <- .Call(P_makeSEIR,params,ivps,t0)
  .Call(P_runSEIR,x,time) |>
    structure(model="SEIR",class=c("gpsim","gpgen"))
}

##' @rdname seir
##' @inheritParams simulate
##' @export
continueSEIR <- function (
  object, time,
  Beta = NA, sigma = NA, gamma = NA, psi = NA, delta = NA
) {
  params <- c(
    Beta=Beta,sigma=sigma,gamma=gamma,psi=psi,delta=delta
  )
  x <- .Call(P_reviveSEIR,object,params)
  .Call(P_runSEIR,x,time) |>
    structure(model="SEIR",class=c("gpsim","gpgen"))
}

##' @name seirs_pomp
##' @rdname seir
##' @include lbdp.R sir.R
##' @param x genealogy in \pkg{phylopomp} format.
##' @details
##' \code{seirs_pomp} constructs a \pkg{pomp} object containing a given set of data and an SEIRS model.
##' @importFrom pomp pomp onestep
##' @export
seirs_pomp <- function (
  x,
  Beta, sigma, gamma, psi, delta = 0,
  S0, E0, I0, R0
)
{
  x |>
    getInfo(
      prune=TRUE,obscure=TRUE,
      nsample=TRUE,lineages=TRUE,genealogy=TRUE
    ) -> geninfo
  ic <- as.integer(c(S0,E0,I0,R0))
  names(ic) <- c("S0","E0","I0","R0")
  if (any(ic < 0))
    pStop(paste(sQuote(names(ic)),collapse=","),
      " must be nonnegative integers.")
  geninfo$lineages |> as.data.frame() -> dat
  nsample <- geninfo$nsample
  pomp(
    data=NULL,
    t0=dat$time[1L],
    times=dat$time[-1L],
    params=c(
      Beta=Beta,sigma=sigma,gamma=gamma,psi=psi,delta=delta,
      ic,N=sum(ic)
    ),
    nstatevars=nsample+8L,
    genealogy=geninfo$genealogy,
    nsample=nsample,
    rinit="seirs_rinit",
    rprocess=onestep("seirs_gill"),
    dmeasure="seirs_dmeas",
    statenames=c("S","E","I","R","ll","node","linE","linI","lineage"),
    paramnames=c(
      "Beta","sigma","gamma","psi","delta",
      "S0","E0","I0","R0","N"
    ),
    PACKAGE="phylopomp"
  )
}

##' Overdispered SIR with sampling simulator.
##'
##' Run the simulator.
##'
##' @name multisirws
##' @aliases multiSIRwS
##' @include getinfo.R multisirws.R
##' 
##' @family Genealogy processes
##' 
##' @inheritParams moran
##' @param data optional data frame; output from \code{playmultiSIRwS}.
##' @param Beta transmission rate
##' @param gamma recovery rate
##' @param psi sampling rate
##' @param theta inverse over-dispersed parameter
##' @param S0 initial size of susceptible population
##' @param I0 initial size of infected population
##' @param R0 initial size of recovered population
##' 
##' @return A \code{tibble} with \code{state} attribute.
##'
##' @example examples/multisirws.R
##'
##' @importFrom dplyr bind_rows filter
##' @importFrom tibble as_tibble
##' @importFrom utils globalVariables
##'
##' @rdname multisirws
##' @export
playmultiSIRwS <- function (
  data = NULL, Beta, gamma, psi, theta = Inf,
  S0, I0, R0,
  t0 = 0, times,
  tree = FALSE, ill = FALSE
) {
  state <- attr(data,"state")
  if (missing(Beta)) Beta <- NULL
  if (missing(gamma)) gamma <- NULL
  if (missing(psi)) psi <- NULL
  if (missing(theta)) theta <- NULL
  if (missing(S0)) S0 <- NULL
  if (missing(I0)) I0 <- NULL
  if (missing(R0)) R0 <- NULL
  x <- .Call(P_playmultiSIRwS,Beta,gamma,psi,theta,S0,I0,R0,times,t0,tree,ill,state)
  state <- x$state
  x$state <- NULL
  data |>
    bind_rows(
      x |> as_tibble() |> filter(!is.na(count))
    ) -> x
  attr(x,"state") <- state
  attr(x,"model") <- "multiSIRwS"
  if (!inherits(x,"gpsim")) class(x) <- c("gpsim",class(x))
  x
}

utils::globalVariables("count")

##' @name multisir_pomp
##' @rdname multisirws
##'
##' @details
##' \code{multisir_pomp} constructs a \pkg{pomp} object containing a given set of data and an overdispersed SIR model.
##'
##' It is assumed that \code{data} is in the format returned by \code{\link{newick2df}}.
##'
##' @importFrom pomp pomp onestep euler covariate_table
##'
##' @export

multisir_pomp <- function (data, Beta, gamma, psi, theta, S0, I0, R0, t0 = 0)
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
      params=c(
        Beta=Beta,gamma=gamma,psi=psi,theta=theta,
        S0=S0,I0=I0,R0=R0,N=S0+I0+R0
      ),
      rinit="multisir_rinit",
      dmeasure="multisir_dmeas",
      paramnames=c("Beta","gamma","psi","theta","S0","I0","R0","N"),
      accumvars=c("ll"),
      statenames=c("S","I","R","ll"),
      PACKAGE="phylopomp",
      covar=covariate_table(
        data,
        times="time",
        order="constant"
      ),
      rprocess=onestep("multisir_gill")
    )
}

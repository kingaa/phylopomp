##' SIRS with sampling simulator.
##'
##' Run the simulator.
##'
##' @name sirs
##' @aliases SIRS
##' @include getinfo.R
##' 
##' @family Genealogy processes
##' 
##' @inheritParams moran
##' @param data optional data frame; output from \code{playSIRwS}.
##' @param Beta transmission rate.
##' @param gamma recovery rate.
##' @param psi sampling rate.
##' @param Delta waning rate of immunity.
##' @param S0 initial size of susceptible population.
##' @param I0 initial size of infected population.
##' @param R0 initial size of recovered population.
##' 
##' @return A \code{tibble} with \code{state} attribute.
##'
##' @importFrom dplyr bind_rows filter
##' @importFrom tibble as_tibble
##' @importFrom utils globalVariables
##'
##' @rdname sirs
##' @export
playSIRS <- function (
  data = NULL,
  Beta, gamma, psi, Delta,
  S0, I0, R0,
  t0 = 0, times,
  tree = FALSE, ill = FALSE
) {
  state <- attr(data,"state")
  if (missing(Beta)) Beta <- NULL
  if (missing(gamma)) gamma <- NULL
  if (missing(psi)) psi <- NULL
  if (missing(Delta)) Delta <- NULL
  if (missing(S0)) S0 <- NULL
  if (missing(I0)) I0 <- NULL
  if (missing(R0)) R0 <- NULL
  x <- .Call(P_playSIRS,Beta,gamma,psi,Delta,S0,I0,R0,times,t0,tree,ill,state)
  state <- x$state
  x$state <- NULL
  data |>
    bind_rows(
      x |> as_tibble() |> filter(!is.na(count))
    ) -> x
  attr(x,"state") <- state
  attr(x,"model") <- "SIRS"
  if (!inherits(x,"gpsim")) class(x) <- c("gpsim",class(x))
  x
}

utils::globalVariables("count")

##' @name sirs_pomp
##' @rdname sirs
##'
##' @param data data frame containing the genealogy in the format returned by \code{\link{newick2df}}.
##' @details
##' \code{sirs_pomp} constructs a \pkg{pomp} object containing a given set of data and a SIR model.
##'
##' @importFrom pomp pomp onestep covariate_table
##'
##' @export

sirs_pomp <- function (data, Beta, gamma, psi, Delta, S0, I0, R0, t0=0)
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
      params=c(Beta=Beta,gamma=gamma,psi=psi,Delta=Delta,
        S0=S0,I0=I0,R0=R0,N=S0+I0+R0),
      rinit="sirs_rinit",
      dmeasure="sirs_dmeas",
      paramnames=c("Beta","gamma","psi","S0","I0","R0","N","Delta"),
      accumvars=c("ll"),
      statenames=c("S","I","R","ll"),
      PACKAGE="phylopomp",
      covar=covariate_table(
        data,
        times="time",
        order="constant"
      ),
      rprocess=onestep("sirs_gill")
    )
}

##' SIR with sampling simulator.
##'
##' Run the simulator.
##'
##' @name sirws
##' @aliases SIRwS
##' @include getinfo.R
##' 
##' @family Genealogy processes
##' 
##' @inheritParams moran
##' @param data optional data frame; output from \code{playSIRwS}.
##' @param Beta transmission rate.
##' @param gamma recovery rate.
##' @param psi sampling rate.
##' @param S0 initial size of susceptible population.
##' @param I0 initial size of infected population.
##' @param R0 initial size of recovered population.
##' @param method numerical integration method.
##' @param delta.t time interval in \dQuote{euler} method.
##' 
##' @return A \code{tibble} with \code{state} attribute.
##'
##' @example examples/sirws.R
##'
##' @importFrom dplyr bind_rows filter
##' @importFrom tibble as_tibble
##' @importFrom utils globalVariables
##'
##' @rdname sirws
##' @export
playSIRwS <- function (
  data = NULL,
  Beta, gamma, psi, S0,
  I0, R0,
  t0 = 0, times,
  tree = FALSE, ill = FALSE
) {
  state <- attr(data,"state")
  if (missing(Beta)) Beta <- NULL
  if (missing(gamma)) gamma <- NULL
  if (missing(psi)) psi <- NULL
  if (missing(S0)) S0 <- NULL
  if (missing(I0)) I0 <- NULL
  if (missing(R0)) R0 <- NULL
  x <- .Call(P_playSIRwS,Beta,gamma,psi,S0,I0,R0,times,t0,tree,ill,state)
  state <- x$state
  x$state <- NULL
  data |>
    bind_rows(
      x |> as_tibble() |> filter(!is.na(count))
    ) -> x
  attr(x,"state") <- state
  attr(x,"model") <- "SIRwS"
  if (!inherits(x,"gpsim")) class(x) <- c("gpsim",class(x))
  x
}

utils::globalVariables("count")

##' @name sir_pomp
##' @rdname sirws
##'
##' @param data data frame containing the genealogy in the format returned by \code{\link{newick2df}}.
##' @param method choice of simulation method.
##' @param delta.t Euler step size.
##'
##' @details
##' \code{sir_pomp} constructs a \pkg{pomp} object containing a given set of data and a SIR model.
##'
##' @importFrom pomp pomp onestep euler covariate_table
##'
##' @export

sir_pomp <- function (data, Beta, gamma, psi, S0, I0, R0, t0=0, 
  method = c("gillespie", "euler"), delta.t = NULL)
{
  method <- match.arg(method)
  delta.t <- as.double(delta.t)
  if (method == "euler" && (length(delta.t)<1 || !is.finite(delta.t) || delta.t < 0))
    stop(sQuote("delta.t")," must be a positive number when method = ",
      dQuote("euler"),".",.call=FALSE)
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
      rprocess=
        if (method=="gillespie") {
          onestep("sir_gill")
        } else {
          euler("sir_euler",delta.t=delta.t)
        }
    )
}

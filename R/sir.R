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
##' @param data optional data frame; output from \code{playSIR}.
##' @param Beta transmission rate.
##' @param gamma recovery rate.
##' @param psi sampling rate.
##' @param S0 initial size of susceptible population.
##' @param I0 initial size of infected population.
##' @param R0 initial size of recovered population.
##' @param t0 initial time
##' @param times times at which output is requested.
##' @param tree logical; represent the genealogical tree in Newick format?
##' @param compact logical; return the tree in compact format?
##' 
##' @return A \code{tibble} with \code{state} attribute.
##'
##' @example examples/sir.R
##'
##' @importFrom dplyr bind_rows filter
##' @importFrom tibble as_tibble
##' @importFrom utils globalVariables
##'
##' @rdname sir
##' @export
playSIR <- function (
  data = NULL,
  Beta, gamma, psi, S0,
  I0, R0,
  t0 = 0, times,
  tree = FALSE, 
  compact = TRUE
) {
  state <- attr(data,"state")
  if (missing(Beta)) Beta <- NULL
  if (missing(gamma)) gamma <- NULL
  if (missing(psi)) psi <- NULL
  if (missing(S0)) S0 <- NULL
  if (missing(I0)) I0 <- NULL
  if (missing(R0)) R0 <- NULL
  x <- .Call(P_playSIR,Beta,gamma,psi,S0,I0,R0,times,t0,tree,compact,state)
  state <- x$state
  x$state <- NULL
  data |>
    bind_rows(
      x |> as_tibble() |> filter(!is.na(count))
    ) -> x
  if (exists("tree",where=x))
    x$tree <- sapply(x$tree,\(t) gsub("nan","NA",t)) 
  attr(x,"state") <- state
  attr(x,"model") <- "SIR"
  if (!inherits(x,"gpsim")) class(x) <- c("gpsim",class(x))
  x
}

utils::globalVariables("count")

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

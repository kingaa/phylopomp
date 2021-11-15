##' SIR with sampling simulator.
##'
##' Run the simulator.
##'
##' @name sirws
##' @aliases SIRwS
##' @include getinfo.R sirws.R
##' 
##' @family Genealogy processes
##' 
##' @inheritParams moran
##' @param data optional data frame; output from \code{playSIRwS}.
##' @param beta transmission rate
##' @param gamma recovery rate
##' @param psi sampling rate
##' @param S0 initial size of susceptible population
##' @param I0 initial size of infected population
##' @param R0 initial size of recovered population
##' @param N initial total population size
##' @param method either \dQuote{gillespie} or \dQuote{euler}
##' @param delta.t time interval in \dQuote{euler} method
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
playSIRwS <- function (data = NULL, ..., beta, gamma, psi, S0, I0, R0, t0 = 0, times,
  tree = FALSE, ill = FALSE) {
  state <- attr(data,"state")
  if (missing(beta)) beta <- NULL
  if (missing(gamma)) gamma <- NULL
  if (missing(psi)) psi <- NULL
  if (missing(S0)) S0 <- NULL
  if (missing(I0)) I0 <- NULL
  if (missing(R0)) R0 <- NULL
  x <- .Call(P_playSIRwS,beta,gamma,psi,S0,I0,R0,times,t0,tree,ill,state)
  state <- x$state
  x$state <- NULL
  data %>%
    bind_rows(
      x %>% as_tibble() %>% filter(!is.na(count))
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
##' @details
##' \code{sir_pomp} constructs a \pkg{pomp} object containing a given set of data and a SIR model.
##'
##' It is assumed that \code{data} is in the format returned by \code{\link{nwk2df}}.
##'
##' @importFrom pomp pomp onestep euler covariate_table
##'
##' @export

sir_pomp <- function (data, beta, gamma, psi, S0, I0, R0, N, t0=0, 
  method = c("gillespie", "euler"), delta.t = NULL)
{
  method <- match.arg(method)
  delta.t <- as.double(delta.t)
  if (method == "euler" && (length(delta.t)<1 || !is.finite(delta.t)))
    stop(sQuote("delta.t")," must be specified when method = ",
      dQuote("euler"),".",.call=FALSE)
  data[,"time"] %>%
    pomp(
      times="time",t0=t0,
      params=c(Beta=beta,gamma=gamma,psi=psi,S0=S0,I0=I0,R0=R0,N=N),
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
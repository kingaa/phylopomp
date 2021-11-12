##' Moran genealogy process.
##'
##' Run the MGP simulator.
##'
##' @name moran
##' @aliases MGP
##' @aliases mgp
##' @rdname moran
##' @include getinfo.R
##'
##' @family Genealogy processes
##'
##' @param data optional data frame; output from \code{playMoran} or \code{playMoranWChain}
##' @param n population size
##' @param mu Moran event rate.
##' @param sample logical;
##' if \code{sample=TRUE}, a sample is taken at each of the specified times.
##' @param stationary logical;
##' should the initial genealogy be drawn from the stationary distribution?
##' @param ill logical; return an illustration?
##' @param ntimes integer; number of timesteps to advance the chain.
##' @param t0 initial time
##' @param times times at which output is requested.
##' @param tree logical; represent the genealogical tree in Newick format?
##' @param ... additional arguments; ignored.
##' 
##' @return A \code{tibble} with \code{state} attribute.
##'
##' @example examples/moran.R
##'
##' @importFrom dplyr bind_rows
##' @importFrom tibble as_tibble
##' @importFrom utils globalVariables
##'
NULL

##' @rdname moran
##' @export
playMoran <- function (data = NULL, ..., n, mu, t0 = 0, times, sample = TRUE,
  tree = FALSE, ill = FALSE, stationary = TRUE) {
  state <- attr(data,"state")
  if (missing(n)) n <- NULL
  if (missing(mu)) mu <- NULL
  if (is.null(data) && is.null(n))
    stop("in ",sQuote("playMoran"),", ",sQuote("n")," is a required argument.",call.=FALSE)
  if (is.null(data) && is.null(mu))
    stop("in ",sQuote("playMoran"),", ",sQuote("mu")," is a required argument.",call.=FALSE)
  x <- .Call(P_playMoran,n,mu,times,t0,sample,tree,ill,stationary,state)
  state <- x$state
  x$state <- NULL
  data |>
    bind_rows(
      x |> as_tibble()
    ) -> x
  attr(x,"state") <- state
  attr(x,"model") <- "Moran"
  if (!inherits(x,"gpsim")) class(x) <- c("gpsim",class(x))
  x
}

##' @rdname moran
##' @export
playMoranWChain <- function (data = NULL, ..., n, mu, t0 = 0, ntimes, tree = TRUE, ill = TRUE, stationary = TRUE) {
  state <- attr(data,"state")
  if (missing(n)) n <- NULL
  if (missing(mu)) mu <- NULL
  if (is.null(data) && is.null(n))
    stop("in ",sQuote("playMoranWChain"),", ",sQuote("n")," is a required argument.",call.=FALSE)
  if (is.null(data) && is.null(mu))
    stop("in ",sQuote("playMoranWChain"),", ",sQuote("mu")," is a required argument.",call.=FALSE)
  x <- .Call(P_playMoranWChain,n,mu,ntimes,t0,tree,ill,stationary,state)
  state <- x$state
  x$state <- NULL
  x$count <- 1
  data |>
    bind_rows(
      x |> as_tibble()
    ) -> x
  attr(x,"state") <- state
  attr(x,"model") <- "Moran"
  if (!inherits(x,"gpsim")) class(x) <- c("gpsim",class(x))
  x
}

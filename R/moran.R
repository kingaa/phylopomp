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
##' @include sirws.R
##' @inheritParams sirws
##' 
##' @param data optional data frame;
##' output from \code{playMoran}.
##' @param n population size
##' @param mu Moran event rate.
##' @param stationary logical;
##' should the initial genealogy be drawn from the stationary distribution?
##' @param ill logical;
##' return an illustration?
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
playMoran <- function (data = NULL, ..., n, mu, t0 = 0, times, tree = FALSE,
  ill = FALSE, stationary = TRUE) {
  state <- attr(data,"state")
  if (missing(n)) n <- NULL
  if (missing(mu)) mu <- NULL
  if (is.null(data) && is.null(n))
    stop("in ",sQuote("playMoran"),", ",sQuote("n")," is a required argument.",call.=FALSE)
  if (is.null(data) && is.null(mu))
    stop("in ",sQuote("playMoran"),", ",sQuote("mu")," is a required argument.",call.=FALSE)
  x <- .Call(P_playMoran,n,mu,times,t0,tree,ill,stationary,state)
  state <- x$state
  x$state <- NULL
  data %>%
    bind_rows(
      x %>% as_tibble()
    ) -> x
  attr(x,"state") <- state
  attr(x,"model") <- "Moran"
  if (!inherits(x,"gpsim")) class(x) <- c("gpsim",class(x))
  x
}

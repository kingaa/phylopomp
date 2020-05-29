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
##' @param mu Moran event rate
##' @param stationary logical;
##' should the initial genealogy be drawn from the stationary distribution?
##' 
##' @return A \code{tibble} with \code{state} attribute.
##'
##' @example examples/moran.R
##'
##' @importFrom dplyr bind_rows filter
##' @importFrom tibble as_tibble tibble
##' @importFrom utils globalVariables
##'
NULL

##' @rdname moran
##' @export
playMoran <- function (data = NULL, ..., n, mu, t0 = 0, times, tree = FALSE,
  stationary = TRUE) {
  state <- attr(data,"state")
  if (missing(n)) n <- NULL
  if (missing(mu)) mu <- NULL
  x <- .Call(P_playMoran,n,mu,times,t0,tree,stationary,state)
  state <- x$state
  x$state <- NULL
  data %>%
    bind_rows(
      x %>% as_tibble()
    ) -> x
  attr(x,"state") <- state
  if (!all(inherits(x,c("Moran_gpsim","gpsim"),TRUE)))
    class(x) <- c("Moran_gpsim","gpsim",class(x))
  x
}

utils::globalVariables("count")

##' @rdname moran
##' @export
getInfo.Moran_gpsim <- function (data, ..., prune  = TRUE, tree = TRUE) {
  x <- .Call(P_get_Moran_info,attr(data,"state"),prune,tree)
  x$cumhaz <- tibble(time=x$time,Lambda=x$cumhaz)
  x$lineages <- tibble(time=x$etimes,lineages=x$lineages)
  attr(x,"state") <- attr(data,"state")
  if (!all(inherits(x,c("Moran_gpsim","gpsim"),TRUE)))
    class(x) <- c("Moran_gpsim","gpsim",class(x))
  x
}

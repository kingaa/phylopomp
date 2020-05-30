##' Linear birth-death process.
##'
##' Run the LBDP simulator.
##'
##' @name lbdp
##' @aliases LBDP
##' @rdname lbdp
##' @include getinfo.R
##'
##' @family Genealogy processes
##'
##' @include sirws.R
##' @inheritParams sirws
##' 
##' @param data optional data frame;
##' output from \code{playLBDP}.
##' @param lambda birth rate
##' @param mu death rate
##' @param psi sampling rate
##' @param n0 initial population size
##' 
##' @return A \code{tibble} with \code{state} attribute.
##'
##' @example examples/lbdp.R
##'
##' @importFrom dplyr bind_rows
##' @importFrom tibble as_tibble tibble
##' @importFrom utils globalVariables
##'
NULL

##' @rdname lbdp
##' @export
playLBDP <- function (data = NULL, ..., lambda, mu, n0, psi, t0 = 0, times, tree = FALSE) {
  state <- attr(data,"state")
  if (missing(n0)) n0 <- NULL
  if (missing(lambda)) lambda <- NULL
  if (missing(mu)) mu <- NULL
  if (missing(psi)) psi <- NULL
  x <- .Call(P_playLBDP,lambda,mu,psi,n0,times,t0,tree,state)
  state <- x$state
  x$state <- NULL
  data %>%
    bind_rows(
      x %>% as_tibble()
    ) -> x
  attr(x,"state") <- state
  if (any(wh <- !inherits(x,c("LBDP_gpsim","gpsim"),TRUE)))
    class(x) <- c(c("LBDP_gpsim","gpsim")[wh],class(x))
  x
}

utils::globalVariables("count")

##' @rdname lbdp
##' @export
getInfo.LBDP_gpsim <- function (data, ..., prune  = TRUE, tree = TRUE) {
  x <- .Call(P_get_LBDP_info,attr(data,"state"),prune,tree)
  x$cumhaz <- tibble(time=x$time,Lambda=x$cumhaz)
  x$lineages <- tibble(time=x$etimes,lineages=x$lineages)
  x$etimes <- NULL
  attr(x,"state") <- attr(data,"state")
  if (any(wh <- !inherits(x,c("LBDP_gpsim","gpsim"),TRUE)))
    class(x) <- c(c("LBDP_gpsim","gpsim")[wh],class(x))
  x
}

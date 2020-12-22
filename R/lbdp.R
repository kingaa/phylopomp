##' Linear birth-death process.
##'
##' Run the LBDP simulator.
##'
##' @name lbdp
##' @aliases LBDP
##' @rdname lbdp
##' @include getinfo.R sirws.R
##'
##' @family Genealogy processes
##'
##' @inheritParams moran
##' @param data optional data frame; output from \code{playLBDP}.
##' @param lambda birth rate
##' @param mu death rate
##' @param psi sampling rate
##' @param n0 initial population size
##' 
##' @return A \code{tibble} with \code{state} attribute.
##'
##' @example examples/lbdp.R
##'
##' @importFrom dplyr bind_rows filter
##' @importFrom tibble as_tibble
##' @importFrom utils globalVariables
##'
NULL

##' @rdname lbdp
##' @export
playLBDP <- function (data = NULL, ..., lambda, mu, n0, psi, t0 = 0, times,
  tree = FALSE, ill = FALSE) {
  state <- attr(data,"state")
  if (missing(n0)) n0 <- NULL
  if (missing(lambda)) lambda <- NULL
  if (missing(mu)) mu <- NULL
  if (missing(psi)) psi <- NULL
  x <- .Call(P_playLBDP,lambda,mu,psi,n0,times,t0,tree,ill,state)
  state <- x$state
  x$state <- NULL
  data %>%
    bind_rows(
      x %>% as_tibble() %>% filter(!is.na(count))
    ) -> x
  attr(x,"state") <- state
  attr(x,"model") <- "LBDP"
  if (!inherits(x,"gpsim")) class(x) <- c("gpsim",class(x))
  x
}

utils::globalVariables("count")

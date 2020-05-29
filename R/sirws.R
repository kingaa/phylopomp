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
##' @param data optional data frame; output from \code{playSIRwS}.
##' @param beta transmission rate
##' @param gamma recovery rate
##' @param psi sampling rate
##' @param S0 initial size of susceptible population
##' @param I0 initial size of infected population
##' @param t0 initial time
##' @param times times at which output is requested.
##' @param tree logical; represent the genealogical tree in Newick format?
##' @param ... additional arguments; ignored.
##' 
##' @return A \code{tibble} with \code{state} attribute.
##'
##' @example examples/sirws.R
##'
##' @importFrom dplyr bind_rows filter
##' @importFrom tibble as_tibble tibble
##' @importFrom utils globalVariables
##'
##' @rdname sirws
##' @export
##' 
playSIRwS <- function (data = NULL, ..., beta, gamma, psi, S0, I0, t0 = 0, times, tree = FALSE) {
  state <- attr(data,"state")
  if (missing(beta)) beta <- NULL
  if (missing(gamma)) gamma <- NULL
  if (missing(psi)) psi <- NULL
  if (missing(S0)) S0 <- NULL
  if (missing(I0)) I0 <- NULL
  x <- .Call(P_playSIRwS,beta,gamma,psi,S0,I0,times,t0,tree,state)
  state <- x$state
  x$state <- NULL
  data %>%
    bind_rows(
      x %>% as_tibble() %>% filter(!is.na(count))
    ) -> x
  attr(x,"state") <- state
  if (!all(inherits(x,c("SIRwS_gpsim","gpsim"),TRUE)))
    class(x) <- c("SIRwS_gpsim","gpsim",class(x))
  x
}

utils::globalVariables("count")

##' @rdname sirws
##' @param prune Prune the tree?
##' @export
getInfo.SIRwS_gpsim <- function (data, ..., prune  = TRUE, tree = TRUE) {
  x <- .Call(P_get_SIRwS_info,attr(data,"state"),prune,tree)
  x$cumhaz <- tibble(Lambda=x$cumhaz)
  x$lineages <- tibble(time=x$etimes,lineages=x$lineages)
  attr(x,"state") <- attr(data,"state")
  if (!all(inherits(x,c("SIRwS_gpsim","gpsim"),TRUE)))
    class(x) <- c("SIRwS_gpsim","gpsim",class(x))
  x
}

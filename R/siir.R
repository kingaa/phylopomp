##' SIIR with sampling simulator.
##'
##' Run the simulator.
##'
##' @name siir
##' @aliases SIIR
##' @include getinfo.R
##' 
##' @family Genealogy processes
##' 
##' @inheritParams sir
##' @param Beta1,Beta2 transmission rates from each of the infectious classes.
##' @param gamma recovery rate.
##' @param psi sampling rate.
##' @param S0 initial size of susceptible population.
##' @param I1_0 initial size of I2 population.
##' @param I2_0 initial size of I2 population.
##' @param R0 initial size of recovered population.
##' 
##' @return A \code{tibble} with \code{state} attribute.
##'
##' @example examples/siir.R
##'
##' @importFrom dplyr bind_rows filter
##' @importFrom tibble as_tibble
##' @importFrom utils globalVariables
##'
##' @rdname siir
##' @export
playSIIR <- function (
  data = NULL,
  Beta1, Beta2, gamma, psi, S0,
  I1_0, I2_0, R0,
  t0 = 0, times,
  tree = FALSE,
  compact = TRUE
) {
  state <- attr(data,"state")
  if (missing(Beta1)) Beta1 <- NULL
  if (missing(Beta2)) Beta2 <- NULL
  if (missing(gamma)) gamma <- NULL
  if (missing(psi)) psi <- NULL
  if (missing(S0)) S0 <- NULL
  if (missing(I1_0)) I1_0 <- NULL
  if (missing(I2_0)) I2_0 <- NULL
  if (missing(R0)) R0 <- NULL
  x <- .Call(P_playSIIR,Beta1,Beta2,gamma,psi,S0,I1_0,I2_0,R0,times,t0,tree,compact,state)
  state <- x$state
  x$state <- NULL
  data |>
    bind_rows(
      x |> as_tibble() |> filter(!is.na(count))
    ) -> x
  if (exists("tree",where=x))
    x$tree <- sapply(x$tree,\(t) gsub("nan","NA",t)) 
  attr(x,"state") <- state
  attr(x,"model") <- "SIIR"
  if (!inherits(x,"gpsim")) class(x) <- c("gpsim",class(x))
  x
}

utils::globalVariables("count")

##' Continue
##'
##' Continue previous run
##'
##' @name continue
##' @include getinfo.R
##' 
##' @family Genealogy processes
##' 
##' @param data previously computed \sQuote{gpsim} object.
##' @param times times at which output is requested.
##' @param ... additional arguments, for example to change model parameters.
##'
##' @section Note:
##' One cannot change initial conditions or \code{t0} with \code{continue}.
##' 
##' @return A \code{tibble} with \code{state} attribute.
##'
##' @importFrom dplyr bind_rows filter
##' @importFrom tibble as_tibble
##' @importFrom utils globalVariables
NULL

##' @rdname continue
##' @export
continue <- function (data, ...) {
  UseMethod("continue",data)
}

##' @rdname continue
##' @method continue gpsim
##' @export
continue.gpsim <- function (data, times, ...) {
  state <- attr(data,"state")
  x <- switch(
    attr(data,"model"),
    SIR = continueSIR(state,times,...),
    SIIR = continueSIIR(state,times,...),
    stop("unrecognized ",sQuote("gpsim")," object.",call.=FALSE)
  )
  state <- x$state
  x$state <- NULL
  x |> as_tibble() |> filter(!is.na(count)) -> x
  attr(x,"state") <- state
  attr(x,"model") <- attr(data,"model")
  class(x) <- c("gpsim",class(x))
  x
}

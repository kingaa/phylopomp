##' simulate
##'
##' Simulate Markov genealogy processes
##'
##' @name simulate
##' @include getinfo.R sir.R siir.R
##' 
##' @family Genealogy processes
##' 
##' @param object previously computed \sQuote{gpsim} object
##' @param model name of model to simulate
##' @param time end timepoint of simulation
##' @param ... additional arguments to the model-specific simulation functions
##'
##' @section Note:
##' One cannot change initial conditions or \code{t0} with \code{continue}.
##' 
##' @return An object of \sQuote{gpsim} class.
##'
NULL

##' @rdname simulate
##' @export
simulate <- function (model, ...) {
  if (missing(model)) stop(sQuote("model")," unspecified.",call.=FALSE)
  model <- as.character(model)
  x <- switch(
    model,
    SIR = runSIR(...),
    SIIR = runSIIR(...),
    stop("unrecognized model: ",sQuote(model),".",call.=FALSE)
  )
  attr(x,"model") <- model
  class(x) <- c("gpsim",class(x))
  x
}

##' @rdname simulate
##' @export
continue <- function (object, ...) {
  UseMethod("continue",object)
}

##' @rdname simulate
##' @method continue gpsim
##' @export
continue.gpsim <- function (object, time, ...) {
  x <- switch(
    paste0("model",as.character(attr(object,"model"))),
    modelSIR = continueSIR(object,time=time,...),
    modelSIIR = continueSIIR(object,time=time,...),
    model = stop("no model attribute detected.",call.=FALSE),
    stop("unrecognized model ",sQuote(attr(object,"model")),call.=FALSE)
  )
  attr(x,"model") <- attr(object,"model")
  class(x) <- c("gpsim",class(x))
  x
}

##' Phylodynamics for POMP models
##' 
##' Simulation and inference of Markov genealogy processes.
##' 
##' @name phylopomp
##' @aliases phylopomp-package
##' @docType package
##' @author Aaron A. King, Qianying Lin
##' @import ggplot2
##' @useDynLib phylopomp, .registration = TRUE, .fixes="P_"
##'
NULL

##' @include package.R
##' @importFrom foreach registerDoSEQ
.onAttach <- function (...) {
  foreach::registerDoSEQ()
}

pStop <- function (..., which = -1L) {
  which <- as.integer(which)
  if (length(which)>0L) {
    fn <- sys.call(which[1L])
    stop("in ",sQuote(fn[[1L]]),": ",...,call.=FALSE)
  } else {
    stop(...,call.=FALSE)
  }
}

pWarn <- function (..., which = -1L) {
  which <- as.integer(which)
  if (length(which)>0L) {
    fn <- sys.call(which[1L])
    warning("in ",sQuote(fn[[1L]]),": ",...,call.=FALSE)
  } else {
    warning(...,call.=FALSE)
  }
}

pMess <- function (..., which = -1L) {
  which <- as.integer(which)
  if (length(which)>0L) {
    fn <- sys.call(which[1L])
    message("NOTE: in ",sQuote(fn[[1L]]),": ",...)
  } else {
    message("NOTE: ",...)
  }
}

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

pStop <- function (fn, ...) {
  fn <- as.character(fn)
  stop("in ",sQuote(fn[1L]),": ",...,call.=FALSE)
}

pStop_ <- function (...) {
  stop(...,call.=FALSE)
}

pWarn <- function (fn, ...) {
  fn <- as.character(fn)
  warning("in ",sQuote(fn[1L]),": ",...,call.=FALSE)
}

pWarn_ <- function (...) {
  warning(...,call.=FALSE)
}

pMess <- function (fn, ...) {
  fn <- as.character(fn)
  message("NOTE: in ",sQuote(fn[1L]),": ",...)
}

pMess_ <- function (...) {
  message("NOTE: ",...)
}

#' Phylodynamics for POMP models
#' 
#' Super cool.
#' 
#' @name phylopomp-package
#' @docType package
#' @author Aaron A. King, Qianying Lin
#' 
#' @import ggplot2
#' @importFrom foreach registerDoSEQ
#' 
#' @useDynLib phylopomp, .registration = TRUE, .fixes="P_"
#' 
NULL

foreach::registerDoSEQ()

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

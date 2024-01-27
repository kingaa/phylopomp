##' Phylodynamics for POMP models
##' 
##' Simulation and inference of Markov genealogy processes.
##' 
##' @name phylopomp
##' @aliases phylopomp-package
##' @author Aaron A. King, Qianying Lin
##' @import ggplot2
##' @useDynLib phylopomp, .registration = TRUE, .fixes="P_"
##'
"_PACKAGE"

##' @include package.R
##' @importFrom foreach registerDoSEQ
.onAttach <- function (...) {
  foreach::registerDoSEQ()
}

pStop <- function (..., who = -1L) {
  if (is.integer(who)) {
    who <- sys.call(who)[[1]]
  }
  who <- as.character(who)
  if (length(who) > 0L)
    stop("in ",sQuote(who[1L]),": ",...,call.=FALSE)
  else
    stop(...,call.=FALSE)
}

pStop_ <- function (...) {
  pStop(...,who=NULL)
}

pWarn <- function (..., who = -1L) {
  if (is.integer(who)) {
    who <- sys.call(who)[[1]]
  }
  who <- as.character(who)
  if (length(who) > 0L)
    warning("in ",sQuote(who[1L]),": ",...,call.=FALSE)
  else
    warning(...,call.=FALSE)
}

pWarn_ <- function (...) {
  pWarn(...,who=NULL)
}

pMess <- function (..., who = -1L) {
  if (is.integer(who)) {
    who <- sys.call(who)[[1]] #nocov
  }
  who <- as.character(who)
  if (length(who) > 0L)
    message("NOTE: in ",sQuote(who[1L]),": ",...)
  else
    message("NOTE: ",...)
}

pMess_ <- function (...) {
  pMess(...,who=NULL)
}

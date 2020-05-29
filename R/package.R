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

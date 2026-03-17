##' phylopompRe
##'
##' Phylodynamic Inference for POMP Models with Reassortment
##'
##' @name phylopompRe-package
##' @aliases phylopompRe
##' @docType package
##' @importFrom methods is
##' @importFrom pomp bake stew freeze
##' @importFrom ape read.tree
##' @useDynLib phylopompRe, .registration = TRUE, .fixes = "P_"
NULL

pStop <- function (...) {
  stop(..., call. = FALSE)
}

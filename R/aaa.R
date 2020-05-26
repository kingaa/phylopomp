##' @include package.R
##' @importFrom foreach registerDoSEQ

.onAttach <- function (...) {
  foreach::registerDoSEQ()
}

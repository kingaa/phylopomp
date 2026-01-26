options(digits=3)
suppressPackageStartupMessages({
  library(phylopomp)
})

check_sampling <- function(r_value, expected_sat) {
  set.seed(10101)
  runLBDP(time=1,lambda=0,mu=0,psi=10,r=r_value,n0=5) |>
    gendat() -> gi
  sample_idx <- gi$nodetype == 1L
  if (!any(sample_idx))
    stop("no sample nodes observed for r=", r_value)
  if (!all(gi$saturation[sample_idx] == expected_sat))
    stop("unexpected sample saturation for r=", r_value)
}

check_sampling(0, 1)
check_sampling(1, 0)

check_bad_r <- function(r_value) {
  ok <- FALSE
  tryCatch(
    runLBDP(time=1,lambda=0,mu=0,psi=1,r=r_value,n0=1),
    error = function(e) ok <<- TRUE
  )
  if (!ok)
    stop("expected error for r=", r_value)
}

check_bad_r(-0.1)
check_bad_r(1.1)

options(digits=3)
suppressPackageStartupMessages({
  library(phylopomp)
})
set.seed(1253219857)

runLBDP(time=1,lambda=0,mu=0,psi=10,r=1,n0=5) |>
  gendat() -> gi
sample_idx <- gi$nodetype == 1L
stopifnot(
  `no samples`=any(sample_idx),
  `bad saturation`=all(gi$saturation[sample_idx] == 0)
)

runLBDP(time=1,lambda=0,mu=0,psi=100,r=0,n0=5) |>
  gendat() -> gi
sample_idx <- gi$nodetype == 1L
stopifnot(
  `no samples`=any(sample_idx),
  `bad saturation`=sum(gi$saturation[sample_idx] == 0)==5
)

try(runLBDP(time=1,lambda=0,mu=0,psi=1,r=-0.1,n0=1))
try(runLBDP(time=1,lambda=0,mu=0,psi=1,r=1.5,n0=1))
try(runLBDP(time=1,lambda=0,mu=0,psi=1,r=Inf,n0=1))
try(runLBDP(time=1,lambda=0,mu=0,psi=1,r=NA,n0=1))

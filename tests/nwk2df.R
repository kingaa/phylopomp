suppressPackageStartupMessages({
  library(phylopomp)
  library(tidyverse)
})

options(digits=3)

playSIRwS(beta=2,gamma=1,psi=2,S0=100,I0=2,R0=0,times=c(0,5),t0=0,tree=TRUE) -> x
y <- getInfo(x)
newick2df(y$tree,time=5,show_branches=TRUE) -> z

# compare to y$lineages
all.equal(
  z |>
    select(time,lineages),
  y$lineages |>
    group_by(time) |>
    summarize(lineages=lineages[n()]) |>
    ungroup() |>
    slice(-n()),
  tolerance=1e-5
)

suppressPackageStartupMessages({
  library(phylopomp)
  library(tidyverse)
})

options(digits=3)

playSIRwS(beta=2,gamma=1,psi=2,S0=100,I0=2,times=c(0,5),t0=0,tree=TRUE) -> x
y <- getInfo(x)
newick2df(y$tree) -> z

# compare to y$lineages
y$lineages %>%
  all.equal(tail(z,-1)[,1:2],tolerance=1e-5) %>%
  stopifnot()

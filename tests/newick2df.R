png(filename="newick2df-%02d.png",res=100,width=6,height=4,units="in")

suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(4963811)
options(digits=3)

runSI2R(time=5) |> newick() -> tree

tree |> newick2df()

tree |> newick2df(t0=10) |> slice(1,2,518,519)

runSIR(time=5) |>
  newick() |>
  newick2df() |>
  lbdp_exact(lambda=4,mu=1,psi=2,n0=2)

try(newick2df())

dev.off()

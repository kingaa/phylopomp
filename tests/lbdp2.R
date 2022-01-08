png(filename="lbdp2-%02d.png",res=100)
suppressPackageStartupMessages({
  library(phylopomp)
  library(tidyverse)
  library(pomp)
})
theme_set(theme_bw())
set.seed(442131820)
options(digits=3)

runLBDP(time=2,lambda=4,n0=1) -> x
x |> plot()

x |>
  lineages() |>
  mutate(
    code=coalesce(lineages-lag(lineages),2)
  ) -> dat

dat |> lbdp_exact(lambda=2,mu=1,psi=1)

replicate(
  n=10,
  dat |>
    lbdp_pomp(lambda=2,mu=1,psi=1) |>
    pfilter(Np=10000) |>
    logLik()
) |>
  logmeanexp(se=TRUE)

dev.off()

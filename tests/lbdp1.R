png(filename="lbdp1-%02d.png",res=100)

suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(847110120)
options(digits=3)

runLBDP(time=2) -> x
x
x |> yaml() -> y
x |>
  lineages() |>
  plot()

simulate("LBDP",lambda=2,mu=1,psi=5,n0=3,time=2) |>
  plot(points=TRUE)

runLBDP(lambda=3,mu=1,psi=1,n0=10,time=1) |>
  simulate(time=10,lambda=0.2,psi=3) |>
  plot(points=TRUE)

try(runLBDP(n0=-5,time=10))

simulate("LBDP",lambda=2,mu=1,psi=5,n0=3,time=2) |>
  plot(points=TRUE,palette=c("#ff0000"))

try(simulate("LBDP",lambda=2,mu=1,psi=5,n0=3,time=2) |>
  plot(points=TRUE,palette=NULL))

dev.off()

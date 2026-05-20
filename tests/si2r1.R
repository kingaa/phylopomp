png(filename="si2r1-%02d.png",res=100)

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(481604604)

runSI2R(time=10)

runSI2R(time=2,Beta=4,kappa=5,gamma=1,chi=2) |>
  simulate(time=5) |>
  plot(obscure=FALSE,points=TRUE)

simulate("SI2R",time=2) -> x
x |> plot(prune=FALSE,obscure=FALSE)
x |> yaml() -> y
x |> lineages(obscure=FALSE) |> plot()

runSI2R(time=2,Beta=4,gamma=1,chi=2,omega=1) |>
  simulate(time=5) |>
  plot(prune=FALSE,obscure=FALSE,points=TRUE)

runSI2R(time=2,pop=100,kappa=10,IL0=0.05,etaL=3,etaH=0.1) |>
  simulate(time=3) |>
  lineages(obscure=FALSE) |>
  plot()

dev.off()

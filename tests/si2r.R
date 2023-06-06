png(filename="si2r1-%02d.png",res=100)

suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(481604604)
options(digits=3)

runSI2R(time=10)

runSI2R(time=2,Beta=2,mu=3,gamma=1,psi1=2,psi2=2) |>
  simulate(time=5) |>
  plot(obscure=FALSE,points=TRUE)

simulate("SI2R",time=2) -> x
x |> plot(prune=FALSE,obscure=FALSE)
x |> yaml() -> y
x |> lineages(obscure=FALSE) |> plot()

runSI2R(time=2,Beta=2,mu=3,gamma=1,psi1=2,psi2=2,delta=1) |>
  simulate(time=5) |>
  plot(prune=FALSE,obscure=FALSE,points=TRUE)

runSI2R(time=2,S0=20,mu=10) |>
  simulate(time=3) |>
  lineages(obscure=FALSE) |>
  plot()

dev.off()

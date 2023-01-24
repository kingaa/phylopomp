png(filename="seir1-%02d.png",res=100)

suppressPackageStartupMessages({
  library(phylopomp)
  library(tidyverse)
})
theme_set(theme_bw())
set.seed(847110120)
options(digits=3)

runSEIR(time=10) -> x
x

runSEIR(Beta=3,sigma=1,gamma=1,psi=5,S0=100,I0=5,time=2) |>
  plot(points=TRUE,obscure=FALSE)

runSEIR(Beta=3,sigma=2,gamma=1,psi=2,S0=100,I0=5,time=1) |>
  simulate(time=100) |>
  plot(points=TRUE,time=NULL,obscure=FALSE)

simulate("SEIR",delta=1,time=20,I0=4) -> x
x |> yaml() -> y
x |> lineages(obscure=FALSE) |> plot()

dev.off()

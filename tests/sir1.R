png(filename="sir1-%02d.png",res=100)

suppressPackageStartupMessages({
  library(tidyverse)
  library(pomp)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(847110120)
options(digits=3)

runSIR(time=10) -> x
x

runSIR(Beta=3,gamma=1,psi=5,S0=100,I0=5,time=2) |>
  plot(points=TRUE)

runSIR(Beta=3,gamma=1,psi=2,S0=100,I0=5,time=1) |>
  simulate(time=100) |>
  plot(points=TRUE,time=NULL)

simulate("SIR",Beta=3,gamma=1,psi=2,delta=1,S0=100,I0=5,time=10) -> x
x |> yaml() -> y
x |> lineages() |> plot()
x |> geneal()

x |>
  sir_pomp(
    Beta=3,gamma=1,psi=2,delta=1,
    S0=100,I0=5,R0=0
  ) |>
  pfilter(Np=5000) -> pf

pf |> logLik()
pf |> plot()

try(runSIR(S0=-10,time=1))
try(
  x |>
    sir_pomp(
      Beta=3,gamma=1,psi=2,delta=1,
      S0=-100,I0=5,R0=0
    )
)

dev.off()

png(filename="sir1-%02d.png",res=100)

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(tidyverse)
  library(pomp)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(847110120)

runSIR(time=10) -> x
x

runSIR(Beta=3,gamma=1,psi=5,S0=100,I0=5,pop=105,time=2) |>
  plot(points=TRUE)

runSIR(Beta=3,gamma=1,psi=2,S0=100,I0=5,pop=105,time=1) |>
  simulate(time=10) |>
  plot(points=TRUE)

simulate("SIR",Beta=3,gamma=1,psi=2,omega=1,S0=100,I0=5,pop=105,time=10) -> x
x |> yaml() -> y
x |> lineages() |> plot()
x |> geneal()

x |>
  sir_pomp(
    Beta=3,gamma=1,psi=2,omega=1,
    S0=100,I0=5,R0=0,pop=105
  ) |>
  pfilter(Np=5000) -> pf

pf |> logLik()
pf |> plot()

try(runSIR(S0=-10,time=1))
try(
  x |>
    sir_pomp(
      Beta=3,gamma=1,psi=2,omega=1,
      S0=-100,I0=5,R0=0,pop=100
    )
)

simulate("SIRS",time=10) |> geneal()

dev.off()

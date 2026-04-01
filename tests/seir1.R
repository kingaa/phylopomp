png(filename="seir1-%02d.png",res=100)

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(tidyverse)
  library(pomp)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(847110120)

runSEIR(time=10,S0=100,E0=5,I0=5,pop=110) -> x
x

runSEIR(
  time=2,Beta=3,sigma=1,gamma=1,psi=5,
  S0=100,E0=5,I0=5,pop=110
) |>
  plot(points=TRUE,obscure=FALSE)

runSEIR(
  time=1,Beta=3,sigma=2,gamma=1,psi=2,
  S0=100,E0=5,I0=5,pop=110
) |>
  simulate(time=10) |>
  plot(points=TRUE,obscure=FALSE)

simulate(
  "SEIR",time=20,omega=1,
  S0=100,E0=5,I0=4,pop=109
) -> x
x |> yaml() -> y
x |> lineages(obscure=FALSE) |> plot()

simulate(
  "SEIR",time=2,omega=1,
  S0=100,E0=5,I0=3,pop=108
) |>
  diagram(obscure=FALSE,prune=TRUE)

try(simulate("SEIR",time=3,omega=1,I0=-0.1))

simulate(
  "SEIR",time=3,
  Beta=4,sigma=1,gamma=1,psi=1,omega=1,
  S0=1,I0=0.1,E0=0.1,R0=0,pop=100
) |>
  seirs_pomp(
    Beta=4,sigma=1,gamma=1,psi=1,omega=1,
    S0=1,I0=0.02,E0=0.02,R0=0,pop=100
  ) |>
  pfilter(Np=10) |>
  logLik()

dev.off()

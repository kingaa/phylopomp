png(filename="strains1-%02d.png",res=100)

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(tidyverse)
  library(pomp)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(481604604)

runStrains(time=10,psi1=1,psi2=1,S0=100)

runStrains(
  S0=100,
  Beta1=10,Beta2=8,Beta3=5,
  gamma=1,
  psi1=2,psi2=2,psi3=0.5,
  time=2
) |>
  plot(points=TRUE,obscure=FALSE)

runStrains(
  S0=200,
  Beta1=10,Beta2=8,Beta3=5,
  gamma=1,psi1=2,psi2=2,psi3=0.5,
  time=1
) |>
  simulate(time=5) -> x

x |>
  plot(obscure=FALSE)

plot_grid(
  x |> plot(),
  x |> plot(obscure=FALSE),
  x |> plot(prune=FALSE),
  x |> plot(prune=FALSE,obscure=FALSE),
  align="hv",axis="bl"
)

x |> yaml() -> y
x |> lineages(prune=FALSE,obscure=FALSE) |> plot()

x |>
  strains_pomp(
    Beta1=10,Beta2=8,Beta3=5,
    gamma=1,psi1=2,psi2=2,psi3=0.5,
    S0=200,I1_0=-10,I2_0=10,I3_0=10,R0=0
  ) |>
  try()

x |>
  strains_pomp(
    Beta1=10,Beta2=8,Beta3=5,
    gamma=1,psi1=2,psi2=2,psi3=0.5,
    S0=200,I1_0=10,I2_0=10,I3_0=10,R0=0
  ) |>
  pfilter(Np=1000) |>
  replicate(n=5) |>
  concat() |>
  freeze(seed=1498493631) -> pf

pf |>
  logLik() |>
  logmeanexp(se=TRUE,ess=TRUE)

dev.off()

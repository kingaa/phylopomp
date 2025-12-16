png(filename="strains1-%02d.png",res=100)

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(phylopomp)
  library(tidyverse)
})
theme_set(theme_bw())
set.seed(481604604)

runStrains(time=10,psi1=1,psi2=1,S0=100)

runStrains(S0=100,Beta1=10,Beta2=8,Beta3=5,gamma=1,psi1=2,psi2=2,psi3=0.5,omega=1,time=2) |>
  plot(points=TRUE,obscure=FALSE)

runStrains(
  S0=100,
  Beta1=10,Beta2=8,Beta3=5,
  gamma=1,psi1=2,psi2=2,psi3=0.5,
  omega=1,
  time=1
) |>
  simulate(time=5) |>
  plot(obscure=FALSE,time=NULL)

simulate("Strains",S0=100,time=2) -> x
plot_grid(
  x |> plot(),
  x |> plot(obscure=FALSE),
  x |> plot(prune=FALSE),
  x |> plot(prune=FALSE,obscure=FALSE),
  align="hv",axis="bl"
)

x |> yaml() -> y
x |> lineages(prune=FALSE,obscure=FALSE) |> plot()

dev.off()

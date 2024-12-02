png(filename="siir1-%02d.png",res=100)

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(phylopomp)
  library(tidyverse)
})
theme_set(theme_bw())
set.seed(481604604)

runSIIR(time=10,psi1=1,psi2=1,S0=100)

runSIIR(Beta1=2,Beta2=4,gamma=1,psi1=2,psi2=2,S0=100,I1_0=5,I2_0=2,time=2) |>
  plot(points=TRUE)

runSIIR(Beta=2,Beta2=5,gamma=1,psi1=2,psi2=2,S0=100,I1_0=5,I2_0=1,time=1) |>
  simulate(time=100) |>
  plot(points=TRUE,time=NULL)

simulate("SIIR",time=2,S0=100,psi1=1,psi2=1) -> x
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

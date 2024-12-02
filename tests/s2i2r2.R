png(filename="s2i2r2-%02d.png",res=100)

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(847110120)

runS2I2R2(
  time=20,
  iota2=0.01,Beta12=0.1,
  I1_0=0,I2_0=0,
  psi1=10,psi2=10,
  omega1=0.2,
  b2=0.02,d2=0.02
) -> x

x |> plot(obscure=FALSE,palette=c("#440154FF","#21908CFF","#ffffff00"))

x |> lineages(obscure=FALSE) |> plot()

simulate(
  "S2I2R2",
  time=1,
  iota1=0.01,iota2=0.01,
  Beta12=0.1,
  S1_0=100,S2_0=1000,
  I1_0=2,I2_0=5,
  psi1=10,psi2=10,
  omega1=2,omega2=1,
  b1=1,d1=1,
  b2=0.5,d2=0.5
) |>
  simulate(time=2) -> x

x |> yaml()

dev.off()

png(filename="s2i2r2-%02d.png",res=100)

suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(847110120)
options(digits=3)

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

dev.off()

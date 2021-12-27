png(filename="si2r1-%02d.png",res=100)
suppressPackageStartupMessages({
  library(phylopomp)
  library(tidyverse)
})
theme_set(theme_bw())
set.seed(481604604)
options(digits=3)

runSI2R(time=10)

runSI2R(time=2,Beta=2,gamma=1,psi1=2,psi2=2) |>
  plot(points=TRUE)

simulate("SI2R",time=2) -> x
x |> plot(prune=FALSE,obscure=FALSE)

dev.off()

png(filename="siir1-%02d.png",res=100)
suppressPackageStartupMessages({
  library(phylopomp)
  library(tidyverse)
})
theme_set(theme_bw())
set.seed(481604604)
options(digits=3)

runSIIR(time=10) -> x
x

runSIIR(Beta1=2,Beta2=4,gamma=1,psi=2,S0=100,I1_0=5,I2_0=2,time=2) |>
  plot(points=TRUE)

runSIIR(Beta=2,Beta2=5,gamma=1,psi=2,S0=100,I1_0=5,I2_0=1,time=1) |>
  simulate(time=100) |>
  plot(points=TRUE,compact=FALSE)

dev.off()

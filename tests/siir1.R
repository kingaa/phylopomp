png(filename="siir1-%02d.png",res=100)
suppressPackageStartupMessages({
  library(phylopomp)
  library(tidyverse)
  library(broom)
  library(doParallel)
  library(doRNG)
})
theme_set(theme_bw())
set.seed(481604604)
options(digits=3)

playSIIR(times=1:10) -> x
x

playSIIR(Beta1=2,Beta2=4,gamma=1,psi=2,S0=100,I1_0=5,I2_0=2,times=1:2,tree=TRUE) |>
  plot(points=TRUE)

playSIIR(Beta=2,Beta2=5,gamma=1,psi=2,S0=100,I1_0=5,I2_0=1,times=100) |>
  getInfo() |>
  plot(points=TRUE)

dev.off()

png(filename="sir1-%02d.png",res=100)
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

playSIR(times=1:10) -> x
x

playSIR(Beta=2,gamma=1,psi=2,S0=100,I0=5,times=1:2,tree=TRUE) |>
  plot(points=TRUE)

playSIR(Beta=2,gamma=1,psi=2,S0=100,I0=5,times=100) |>
  getInfo(tree=TRUE,compact=TRUE) |>
  plot(points=TRUE)

dev.off()

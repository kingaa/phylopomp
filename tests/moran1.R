png(filename="moran1-%02d.png",res=100)
suppressPackageStartupMessages({
  library(phylopomp)
  library(tidyverse)
})
theme_set(theme_bw())
set.seed(847110120)
options(digits=3)

runMoran(time=10)

runMoran(n=100,time=100) |>
  plot()

simulate("Moran",time=100,psi=0) |>
  simulate(time=105,psi=1) |>
  plot()

dev.off()

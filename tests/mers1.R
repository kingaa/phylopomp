png(filename="mers1-%02d.png",res=100)

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(481604604)

runMERS(time=10)

runMERS(time=1,Ic0=0.0002,Beta_hc=4,Beta_hh=2,chi_h=2) |>
  simulate(time=2) |>
  plot(obscure=FALSE)

simulate("MERS",time=2,Ic0=0.0002,Beta_hc=4,Beta_hh=2,chi_h=2) -> x
x |> plot(prune=FALSE,obscure=FALSE)
x |> yaml() -> y
x |> lineages(obscure=FALSE) |> plot()

runMERS(time=2,Ic0=0.0002,Beta_hc=4,Beta_hh=2,chi_h=2,Bh=150,Bc=500) |>
  simulate(time=5) |>
  plot(prune=FALSE,obscure=FALSE)

runMERS(time=2,Ic0=0.0005,Beta_hc=0,Beta_hh=5,chi_h=2) |>
  simulate(time=3,Beta_hc=2,Beta_ch=2) |>
  simulate(time=5,Beta_hc=4,Beta_ch=0) |>
  lineages(obscure=FALSE) |>
  plot()

dev.off()

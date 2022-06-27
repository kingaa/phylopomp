library(phylopomp)
library(tidyverse)
theme_set(theme_bw())
set.seed(847110120)
options(digits=3)

simulate("LBDPwr",lambda=2,mu=1,psi=3,rhoA=.5,rhoB=.8,n0=1,time=1)

simulate("LBDPwr",lambda=2,mu=1,psi=3,rhoA=.5,rhoB=.8,n0=1,time=1) |>
  simulate(time=3,lambda=1) |>
  plot(points=TRUE)

library(tidyverse)
library(pomp)
library(phylopomp)

simulate(
  "TwoSpecies",
  t0=0,time=5,
  iota1=0,iota2=0,
  Beta11=4,Beta12=1,
  Beta21=0,Beta22=4,
  gamma1=1,gamma2=1,
  c1=1,c2=1,
  psi1=1,psi2=1,
  S1_0=100,I1_0=2,R1_0=0,
  S2_0=100,I2_0=2,R2_0=0,
  b1=0,b2=0,
  d1=0,d2=0,
  omega1=0,omega2=0
) |>
  freeze(seed=747318644) -> s

s |>
  twospecies_pomp(
    Beta11=4,Beta12=1,
    Beta21=0,Beta22=4,
    gamma1=1,gamma2=1,
    c1=1,c2=1,
    psi1=1,psi2=1,
    S1_0=100,I1_0=2,R1_0=0,
    S2_0=100,I2_0=2,R2_0=0,
    b1=0,b2=0,
    d1=0,d2=0,
    omega1=0,omega2=0
  ) -> p

p |>
  pfilter(Np=1000) |>
  replicate(n=5) |>
  concat() |>
  freeze(seed=468314464) -> pf

pf |>
  logLik() |>
  logmeanexp(se=TRUE,ess=TRUE)

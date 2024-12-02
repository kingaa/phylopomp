png(filename="lbdp2-%02d.png",res=100)

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(tidyverse)
  library(pomp)
  library(phylopomp)
})
theme_set(theme_bw())

## Example 1
runLBDP(time=2,lambda=4,mu=1,psi=1,n0=1) |>
  freeze(seed=302190821) -> x
x |> plot()

x |> lbdp_exact(lambda=6,mu=1,psi=1) -> llex
llex

x |>
  lbdp_pomp(lambda=6,mu=1,psi=1) |>
  pfilter(Np=10000) |>
  replicate(n=10) |>
  concat() |>
  freeze(seed=590246054) -> pfs

pfs |>
  plot(type="s")

pfs |>
  logLik() |>
  logmeanexp(se=TRUE,ess=TRUE) -> llpf
llpf

stopifnot(
  llex>llpf[1]-2*llpf[2],
  llex<llpf[1]+2*llpf[2]
)

## Example 2
runLBDP(time=2,lambda=2,mu=1,psi=2,n0=10) |>
  freeze(seed=813057496) -> x
x |> plot(points=TRUE)
x |> lbdp_exact(lambda=2,mu=1,psi=2,n0=10) -> llex; llex

x |>
  lbdp_pomp(lambda=2,mu=1,psi=2,n0=10) |>
  pfilter(Np=10000) |>
  logLik() |>
  replicate(n=10) |>
  logmeanexp(se=TRUE) |>
  freeze(seed=2122992313) -> llpf; llpf

stopifnot(
  llex>llpf[1]-2*llpf[2],
  llex<llpf[1]+2*llpf[2]
)

## Example 3
freeze(
  seed=362430640,
  runLBDP(time=2,lambda=2,mu=1,psi=2,n0=10)
) -> x
x |> plot(points=TRUE)
x |> lbdp_exact(lambda=2,mu=1,psi=3,n0=10) -> llex; llex

x |>
  lbdp_pomp(lambda=2,mu=1,psi=3,n0=10) |>
  pfilter(Np=10000) |>
  logLik() |>
  replicate(n=10) |>
  logmeanexp(se=TRUE) |>
  freeze(seed=1569852047) -> llpf; llpf

stopifnot(
  llex>llpf[1]-2*llpf[2],
  llex<llpf[1]+2*llpf[2]
)

try(
  x |> lbdp_pomp(n0=-10)
)

dev.off()

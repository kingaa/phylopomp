png(filename="lbdp2-%02d.png",res=100)

suppressPackageStartupMessages({
  library(tidyverse)
  library(pomp)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(442131820)
options(digits=3)

## Example 1
runLBDP(time=2,lambda=4,mu=1,psi=1,n0=1) -> x
x |> plot()
x |> lineages() -> dat

dat |> lbdp_exact(lambda=6,mu=1,psi=1) -> llex
llex

dat |>
  lbdp_pomp(lambda=6,mu=1,psi=1) |>
  pfilter(Np=10000) |>
  replicate(n=10) |>
  concat() -> pfs

pfs |>
  plot(type="h")

pfs |>
  logLik() |>
  logmeanexp(se=TRUE) -> llpf
llpf

stopifnot(
  llex>llpf[1]-2*llpf[2],
  llex<llpf[1]+2*llpf[2]
)

## Example 2
runLBDP(time=2,lambda=2,mu=1,psi=2,n0=10) -> x
x |> plot(points=TRUE)
x |> lineages() -> dat

dat |> lbdp_exact(lambda=2,mu=1,psi=2,n0=10) -> llex; llex

replicate(
  n=10,
  dat |>
    lbdp_pomp(lambda=2,mu=1,psi=2,n0=10) |>
    pfilter(Np=10000) |>
    logLik()
) |>
  logmeanexp(se=TRUE) -> llpf; llpf

stopifnot(
  llex>llpf[1]-2*llpf[2],
  llex<llpf[1]+2*llpf[2]
)

## Example 3
runLBDP(time=2,lambda=2,mu=1,psi=2,n0=10) -> x
x |> plot(points=TRUE)
x |> lineages() -> dat

dat |> lbdp_exact(lambda=2,mu=1,psi=3,n0=10) -> llex; llex

replicate(
  n=10,
  dat |>
    lbdp_pomp(lambda=2,mu=1,psi=3,n0=10) |>
    pfilter(Np=10000) |>
    logLik()
) |>
  logmeanexp(se=TRUE) -> llpf; llpf

stopifnot(
  llex>llpf[1]-2*llpf[2],
  llex<llpf[1]+2*llpf[2]
)

try(
  dat |> lbdp_pomp(n0=-10)
)

dev.off()

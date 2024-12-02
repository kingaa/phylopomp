png(filename="moran1-%02d.png",res=100)

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(847110120)

runMoran(time=10,n=100,mu=1,psi=1) -> x
x
x |> yaml() -> y
x |> lineages()

x |>
  moran_exact() |>
  round(1) |>
  all.equal(-1164.6) |>
  stopifnot()

runMoran(n=100,time=100) |>
  plot()

simulate("Moran",time=100,psi=0) |>
  simulate(time=105,psi=1) |>
  plot()

expand_grid(
  psi=seq(0.1,2,length=40),
  n=100,
  mu=seq(0.4,1.5,length=40)
) |>
  rowwise() |>
  mutate(
    logLik=moran_exact(x,n=n,mu=mu,psi=psi)
  ) |>
  ungroup() |>
  filter(is.finite(logLik)) |>
  ggplot(aes(x=mu,y=psi,z=logLik))+
  geom_tile(aes(fill=logLik),color=NA)+
  geom_contour(color="white",binwidth=50)+
  geom_vline(xintercept=1,color="red")+
  geom_hline(yintercept=1,color="red")+
  theme_bw()

expand_grid(
  psi=seq(0.1,2,length=40),
  n=seq(70,140,length=40),
  mu=1
) |>
  rowwise() |>
  mutate(
    logLik=moran_exact(x,n=n,mu=mu,psi=psi)
  ) |>
  ungroup() |>
  filter(is.finite(logLik)) |>
  ggplot(aes(x=n,y=psi,z=logLik))+
  geom_tile(aes(fill=logLik),color=NA)+
  geom_contour(color="white",binwidth=50)+
  geom_vline(xintercept=100,color="red")+
  geom_hline(yintercept=1,color="red")+
  theme_bw()

dev.off()

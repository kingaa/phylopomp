png(filename="lbdp3-%02d.png",res=100)

suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
  library(pomp)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(442131820)
options(digits=3)

freeze(
  runLBDP(time=1,lambda=4,mu=1,psi=2,n0=2),
  seed=162343023
) -> x

plot_grid(
  x |> plot(points=TRUE,obscure=FALSE),
  x |> diagram(prune=TRUE,obscure=FALSE),
  x |> lineages() |> plot(),
  ncol=1,
  align="v",axis="tblr",
  rel_heights=c(2,1,2)
)

plot_grid(
  x |> plot(points=TRUE,obscure=TRUE),
  x |> diagram(prune=TRUE,obscure=TRUE),
  x |> lineages() |> plot(),
  ncol=1,
  align="v",axis="tblr",
  rel_heights=c(2,1,2)
)

x |>
  lineages() |>
  mutate(
    sample=lineages<=lag(lineages),
    sample=if_else(row_number()<n(),sample,NA),
    sample=coalesce(sample,FALSE)
  ) -> dat

stopifnot(
  sum(dat$sample)==14
)

plot_grid(
  x |> plot(points=TRUE,obscure=TRUE),
  x |> diagram(prune=TRUE,obscure=TRUE),
  dat |>
    ggplot(aes(x=time,y=lineages))+
    geom_step()+
    geom_point(aes(alpha=sample),color="blue",show.legend=FALSE),
  ncol=1,
  align="v",axis="tblr",
  rel_heights=c(2,1,2)
)

dat |>
  lbdp_exact(lambda=4,mu=1,psi=2,n0=2) -> llex
llex

  dat |>
  lbdp_pomp(lambda=4,mu=1,psi=2,n0=2) |>
  pfilter(Np=1000) |>
  logLik() |>
  replicate(n=10) |>
  logmeanexp(se=TRUE) -> llpf
llpf

stopifnot(
  llex>llpf[1]-2*llpf[2],
  llex<llpf[1]+2*llpf[2]
)

dev.off()

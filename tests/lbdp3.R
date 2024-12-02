png(filename="lbdp3-%02d.png",res=100)

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(tidyverse)
  library(cowplot)
  library(pomp)
  library(phylopomp)
})
theme_set(theme_bw())

runLBDP(time=1,lambda=4,mu=1,psi=2,n0=2) |>
  freeze(seed=162343023) -> x

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

x |> lineages() -> dat

stopifnot(
  sum(dat$event_type==-1)==1,
  sum(dat$event_type==1)==14,
  sum(dat$event_type==2)==7,
  sum(dat$event_type==3)==1
)

plot_grid(
  x |> plot(points=TRUE,obscure=TRUE),
  x |> diagram(prune=TRUE,obscure=TRUE),
  dat |>
    mutate(sample=event_type==1) |>
    ggplot(aes(x=time,y=lineages))+
    geom_step()+
    geom_point(aes(alpha=sample),color="blue",show.legend=FALSE),
  ncol=1,
  align="v",axis="tblr",
  rel_heights=c(2,1,2)
)

x |>
  lbdp_exact(lambda=4,mu=1,psi=2,n0=2) -> llex
llex

x |>
  lbdp_pomp(lambda=4,mu=1,psi=2,n0=2) |>
  pfilter(Np=1000) |>
  logLik() |>
  replicate(n=10) |>
  logmeanexp(se=TRUE) |>
  freeze(seed=442131820) -> llpf
llpf

stopifnot(
  llex>llpf[1]-2*llpf[2],
  llex<llpf[1]+2*llpf[2]
)

try(
  x |>
    lbdp_exact(lambda=4,mu=1,psi=2,n0=0)
)

dev.off()

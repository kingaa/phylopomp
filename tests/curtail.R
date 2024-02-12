png(filename="curtail-%02d.png",res=100,width=6,height=4,units="in")

suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(24963811)
options(digits=3)

runSEIR(time=5,S0=20,omega=2) -> x

plot_grid(
  x |>
    plot(points=TRUE,prune=FALSE,obscure=FALSE,ladderize=FALSE)+
    geom_vline(xintercept=3.5),
  x |> lineages(obscure=FALSE,prune=FALSE) |>
    plot()+
    guides(color="none")+
    geom_vline(xintercept=3.5)+
    expand_limits(x=5,y=10),
  x |> curtail(time=3.5) |>
    plot(points=TRUE,prune=FALSE,obscure=FALSE,ladderize=FALSE)+
    geom_vline(xintercept=3.5)+
    expand_limits(x=5),
  x |> curtail(time=3.5) |>
    lineages(obscure=FALSE,prune=FALSE) |>
    plot()+geom_vline(xintercept=3.5)+
    guides(color="none")+
    expand_limits(x=5,y=10),
  ncol=2,
  align="hv",axis="tblr"
)

x |>
  curtail(time=0) |>
  getInfo(time=TRUE,prune=FALSE,obscure=FALSE,newick=TRUE)

x |>
  curtail(time=-1) |>
  getInfo(time=TRUE,prune=FALSE,obscure=FALSE,newick=TRUE)

plot_grid(
  x |>
    curtail(time=1) |>
    diagram(prune=FALSE,obscure=FALSE),
  x |>
    curtail(time=0.5) |>
    diagram(prune=FALSE,obscure=FALSE),
  ncol=1
)

simulate("SEIR",time=0.2) -> x
x |> simulate(time=0.4) -> y

plot_grid(
  y |> diagram(prune=FALSE,obscure=FALSE),
  x |> diagram(prune=FALSE,obscure=FALSE),
  y |> curtail(time=0.2) |> diagram(prune=FALSE,obscure=FALSE),
  ncol=1,rel_heights=c(17,28,28)
)

dev.off()

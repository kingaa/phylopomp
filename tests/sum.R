png(filename="sum-%02d.png",res=100)

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(847110120)

runSEIR(t0=-1,time=2,S0=100,E0=0,I0=3,pop=103) -> x
runSEIR(t0=0,time=3,S0=100,E0=3,I0=0,pop=103) -> y
runSEIR(t0=1,time=4,S0=100,E0=8,I0=0,pop=108) -> z

plot_grid(
  x |> plot(points=TRUE,obscure=FALSE,ladderize=FALSE),
  y |> plot(points=TRUE,obscure=FALSE,ladderize=FALSE),
  z |> plot(points=TRUE,obscure=FALSE,ladderize=FALSE),
  ncol=1
)

(x+x) |>
  plot(points=TRUE,ladderize=TRUE,obscure=FALSE)

(x+y) |>
  plot(points=TRUE,ladderize=FALSE,obscure=FALSE)

(y+x) |>
  plot(points=TRUE,ladderize=FALSE,obscure=FALSE)

(x+y+x) |>
  plot(points=TRUE,ladderize=FALSE,obscure=FALSE)

geneal_sum(x,y,z) |>
  plot(points=TRUE,ladderize=FALSE,obscure=FALSE)

geneal_sum(list(y,z),x) |>
  plot(points=TRUE,ladderize=FALSE,obscure=FALSE)

geneal_sum(list(x,list(y,z))) |>
  plot(points=TRUE,ladderize=FALSE,obscure=FALSE)

geneal_sum(list(x,list(y,z))) |>
  lineages(obscure=FALSE) |>
  plot()

try(geneal_sum(list(x,NULL,list(y,z))))
try(geneal_sum(list(x,list("bob",y,z))))

stopifnot(
  identical(
    geneal_sum(x) |>
      newick(extended=FALSE),
    geneal_sum(list(list(),list(list(list(x)),list(),list()))) |>
      newick(extended=FALSE)
  ),
  geneal_sum() |> newick()=="",
  geneal_sum(list()) |> newick()==""
)

dev.off()

library(tidyverse)
library(cowplot)

playMoran(n=5,mu=5,times=0:10,t0=0,tree=TRUE,ill=TRUE) -> x
playMoran(x,times=11:20,tree=TRUE) -> x
plot(x)

playMoran(n=5,mu=10,times=0:10,t0=-3) |>
  getInfo() -> y
plot(y,points=TRUE,diagram=TRUE)

playMoran(n=20,mu=20,times=0:20,stationary=FALSE,tree=TRUE,ill=TRUE) -> x
plot(x,points=TRUE)

y <- getInfo(x,prune=FALSE)
plot(y,points=TRUE)

playMoran(n=5,mu=5,t0=-1,times=0:3,stationary=FALSE,tree=TRUE,ill=TRUE) -> x
plot(x,points=TRUE,diagram=TRUE)

y <- getInfo(x)
plot(y,points=TRUE,diagram=TRUE)

playMoran(n=20,mu=10,times=1:10,sample=TRUE,stationary=FALSE) -> x
x |> getInfo(prune=FALSE,compact=TRUE) -> y
plot(y,points=TRUE,diagram=TRUE)

x |> getInfo(prune=TRUE,compact=TRUE) -> y
plot(y,points=TRUE,diagram=TRUE)

playMoran(n=8,mu=8,times=0,tree=TRUE,ill=TRUE,sample=FALSE,stationary=TRUE) |>
  playMoranWChain(ntimes=4) |>
  mutate(diag=diagram(illus)) |>
  pull(diag) |>
  {\(x)plot_grid(plotlist=x,ncol=1)}()

playMoran(n=8,mu=8,times=0,tree=TRUE,ill=TRUE,sample=FALSE,stationary=FALSE) |>
  playMoranWChain(ntimes=40) |>
  mutate(diag=diagram(illus)) |>
  slice(35:40) |>
  pull(diag) |>
  {\(x)plot_grid(plotlist=x,ncol=1)}()

png(filename="gendat-%0d.png",res=100,
  width=1114,height=233,units="px")

options(tidyverse.quiet=TRUE,digits=3)
library(tidyverse)
library(phylopomp)
set.seed(1888056571)

runSIRS(time=4,psi=0.3) -> x

plot(x,points=TRUE)

diagram(x)

x |> gendat() -> g
stopifnot(identical(g,getInfo(x,gendat=TRUE)$gendat))
g

g |>
  with({
    stopifnot(
      diff(c(index,max(index)))==saturation
    )
    for (i in seq_len(length(ancestor))) {
      if (saturation[i] > 0) {
        k <- seq.int(from=index[i],to=index[i]+saturation[i]-1,by=1)
        stopifnot(i==ancestor[child[k+1]+1]+1)
      }
    }
  })

freeze(
  seed=540737457,
  runSEIRS(time=40,omega=3,psi=0.3)
) |>
  gendat(obscure=FALSE) -> g

g |>
  with({
    stopifnot(
      diff(c(index,max(index)))==saturation
    )
    for (i in seq_len(length(ancestor))) {
      if (saturation[i] > 0) {
        k <- seq.int(from=index[i],to=index[i]+saturation[i]-1,by=1)
        stopifnot(i==ancestor[child[k+1]+1]+1)
      }
    }
  })

g |>
  _[c("nodetype","deme","child","ancestor")] |>
  bind_cols() |>
  mutate(name=seq_along(deme)-1) |>
  filter(nodetype==1) |>
  count(deme)

freeze(
  seed=540737457,
  runTwoSpecies(time=4,psi1=8,psi2=5,Beta12=10)
) |>
  gendat(obscure=FALSE) -> g

g |>
  with({
    stopifnot(
      diff(c(index,max(index)))==saturation
    )
    for (i in seq_len(length(ancestor))) {
      if (saturation[i] > 0) {
        k <- seq.int(from=index[i],to=index[i]+saturation[i]-1,by=1)
        stopifnot(i==ancestor[child[k+1]+1]+1)
      }
    }
  })

g |>
  _[c("nodetype","deme","child","ancestor")] |>
  bind_cols() |>
  mutate(name=seq_along(deme)-1) |>
  filter(nodetype==1) |>
  count(deme)

freeze(
  seed=540737457,
  runStrains(time=2,psi1=2,psi2=2,psi3=3)
) -> x

x |> gendat(obscure=FALSE) -> g
stopifnot(identical(g,getInfo(x,obscure=FALSE,gendat=TRUE)$gendat))
stopifnot(identical(g,getInfo(x,prune=FALSE,obscure=FALSE,gendat=TRUE)$gendat))
gi <- getInfo(x,nsample=TRUE,nroot=TRUE)
stopifnot(identical(g$nsample,gi$nsample))
stopifnot(identical(g$nroot,gi$nroot))

g |>
  _[c("nodetype","deme","child","ancestor")] |>
  bind_cols() |>
  mutate(name=seq_along(deme)-1) |>
  filter(nodetype==1) |>
  count(deme)

dev.off()

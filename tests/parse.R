png(filename="parse-%02d.png",res=100,width=6,height=4,units="in")

suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(4963811)
options(digits=3)

runSEIR(time=3,I0=3) |>
  getInfo(tree=TRUE,prune=FALSE,obscure=FALSE) |>
  getElement("tree") -> tree
plot_grid(
  tree |> treeplot(points=TRUE),
  tree |> parse_newick(prune=FALSE,obscure=FALSE,tree=TRUE) |>
    getElement("tree") |>
    treeplot(points=TRUE),
  nrow=1
)

runSEIR(time=3,I0=3) |>
  getInfo(tree=TRUE,prune=FALSE,obscure=TRUE) |>
  getElement("tree") -> tree
plot_grid(
  tree |> treeplot(points=TRUE),
  tree |> parse_newick(prune=FALSE,obscure=TRUE,tree=TRUE) |>
    getElement("tree") |>
    treeplot(points=TRUE),
  nrow=1
)

runSEIR(time=30,I0=3) |>
  getInfo(tree=TRUE,prune=FALSE,obscure=TRUE) |>
  getElement("tree") -> tree
plot_grid(
  tree |> treeplot(points=TRUE),
  tree |> parse_newick(prune=FALSE,obscure=TRUE,tree=TRUE) |>
    getElement("tree") |>
    treeplot(points=TRUE),
  nrow=1
)

runSEIR(time=3,I0=3) |>
  getInfo(tree=TRUE,prune=TRUE,obscure=FALSE) |>
  getElement("tree") -> tree
plot_grid(
  tree |> treeplot(points=TRUE),
  tree |> parse_newick(prune=TRUE,obscure=FALSE,tree=TRUE) |>
    getElement("tree") |>
    treeplot(points=TRUE),
  nrow=1
)

runSEIR(time=5,I0=3) -> x
x |>
  getInfo(tree=TRUE,prune=TRUE,obscure=TRUE) |>
  getElement("tree") -> tree
plot_grid(
  x |> plot(points=TRUE,prune=TRUE,obscure=TRUE),
  tree |> parse_newick(prune=TRUE,obscure=TRUE,tree=TRUE) |>
    getElement("tree") |>
    treeplot(points=TRUE),
  nrow=1
)

runSEIR(time=5,I0=3) -> x

plot_grid(
  x |> plot(prune=TRUE,obscure=FALSE,points=TRUE),
  x |> lineages(prune=TRUE,obscure=FALSE) |> plot(legend.position="none"),
  x |> getInfo(tree=TRUE,prune=TRUE,obscure=FALSE) |>
    getElement("tree") |>
    parse_newick(prune=TRUE,obscure=FALSE,lineages=TRUE) |>
    getElement("lineages") |>  
    plot(legend.position="none")+
    expand_limits(x=5),
  align="hv",axis="tblr",
  ncol=1,rel_heights=c(2,1,1)
)

stopifnot(
  all.equal(
    x |> lineages(prune=FALSE,obscure=FALSE) |>
      slice(1:20),
    x |> getInfo(tree=TRUE,prune=FALSE,obscure=FALSE) |>
      getElement("tree") |>
      parse_newick(prune=FALSE,obscure=FALSE,lineages=TRUE) |>
      getElement("lineages") |>
      slice(1:20),
    tolerance=1e-4
  )
)

dev.off()

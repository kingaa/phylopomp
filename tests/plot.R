png(filename="plot-%02d.png",res=100)

library(phylopomp)
set.seed(137429846)

try(treeplot())

simulate("SIR",time=10) |>
  getInfo(tree=TRUE,prune=FALSE) |>
  getElement("tree") |>
  treeplot(time=0,t0=NA)

dev.off()

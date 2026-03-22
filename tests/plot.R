png(filename="plot-%02d.png",res=100)

library(phylopomp)
set.seed(137429846)

try(phylopomp:::treeplot())

simulate("SIR",time=10) |>
  newick(prune=FALSE) |>
  phylopomp:::treeplot(time=0,t0=NA)

phylopomp:::treeplot("")

dev.off()

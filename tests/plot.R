library(phylopomp)
set.seed(137429846)

try(phylopomp:::treeplot())

phylopomp:::treeplot("",time=0,t0=0) -> pl

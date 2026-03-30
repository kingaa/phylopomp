library(phylopomp)
set.seed(137429846)

try(phylopomp:::treeplot())

"" |> parse_newick() |> plot(obscure=FALSE,prune=FALSE,points=TRUE)

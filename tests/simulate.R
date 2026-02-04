library(phylopomp)
try(simulate("billyjoe"))
try(simulate(22))
simulate("SEIR",time=4) -> x
attr(x,"model") <- "billybob"
try(simulate(x,time=10))
attr(x,"model") <- NULL
try(simulate(x,time=10))
attr(x,"model") <- ""
try(simulate(x,time=10))
simulate()

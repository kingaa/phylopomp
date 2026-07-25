options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(phylopomp)
})
set.seed(847110120)

simulate(
  "SEIR",
  Beta=100,pop=100,
  time=1
) -> x

x |> cblv() -> y1
y1

x |> getInfo(cblv=TRUE) -> y2
stopifnot(y1==y2$cblv)

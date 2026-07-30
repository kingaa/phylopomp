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
stopifnot(all(cblv(parse_cblv(y1,0,1))==y1))

x |> getInfo(cblv=TRUE) -> y2
stopifnot(y1==y2$cblv)

n1 <- parse_newick("(a:4,(:1,(b:2,c:1):1):2):1;(:8,:3):1;")
x1 <- cblv(n1)
n2 <- parse_newick("(:8,:3):1;(a:4,(:1,(b:2,c:1):1):2):1;")
x2 <- cblv(n2)
n3 <- parse_newick("(:8,:3):1;((:1,(:2,:1):1):2,:4):1;")
x3 <- cblv(n3)
n4 <- parse_newick("(:8,:3):1;(((:2,:1):1,:1):2,:4):1;")
x4 <- cblv(n4)
stopifnot(
  x1 == x2,
  x2 == x3,
  x3 == x4
)

n <- parse_newick("((a:4,((b:2,c:1):1)):0.5,d:2.5):0.5;")
x = cblv(n)
x
stopifnot(
  x[,1] == c(5.0,3.0,1.0,2.5),
  x[,2] == c(1.0,2.0,0.5,0.0)
)

n <- parse_newick("((a:4,((b:2,c:1):1)):0.5,d:2.5):0;")
x = cblv(n)
x
stopifnot(
  x[,1] == c(4.5,3.0,1.0,2.5),
  x[,2] == c(0.5,1.5,0.0,0.0)
)

## one ternary node, one zero-length branch
n1 <- parse_newick("((a:4,((b:2,c:1,e:3):1)):1,d:3):1;")
x1 <- cblv(n1)
stopifnot(
  x1[,1] == c(6.0,2.0,1.0,4.0,3.0),
  x1[,2] == c(3.0,3.0,2.0,1.0,0.0)
)

## one ternary node, without zero-length branch
n2 <- parse_newick("((a:4,(b:2,c:1,e:3):1):1,d:3):1;")
x2 <- cblv(n2)
stopifnot(x1==x2)

n <- c(
  "((((:0.041,(:0.044,:0.32):0.62):0.2,((:0.35,:0.71):0.058,:0.21):0.54):0.064,(:0.54,:0.99):0.37):0.091);",
  "((((:0.33,:0.93):0.076,(:0.11,:0.27):0.57):0.039,(:0.2,:0.6):0.46):0.34);",
  "((((:0.091,:0.14):0.089,:0.1):0.037,:0.36):0.86);"
)
x <- cblv(parse_newick(paste(n,collapse="")))
x1 <- cblv(parse_newick(n[1]))
x2 <- cblv(parse_newick(n[2]))
x3 <- cblv(parse_newick(n[3]))
stopifnot(
  x == rbind(x1,x2,x3),
  all(cblv(parse_cblv(x,0,1.5))==x),
  all(cblv(parse_cblv(x1,0,1.5))==x1),
  all(cblv(parse_cblv(x2,0,1.5))==x2),
  all(cblv(parse_cblv(x3,0,1.5))==x3)
)

x <- matrix(c(3,2,1,1,2,0),3,2)
parse_cblv(x,-1,3) |> getInfo(time=TRUE,t0=TRUE)
try(parse_cblv(x[,1],0,3))
try(parse_cblv(x[1:2,],0,3))
try(parse_cblv(x,0,2))
x <- matrix(c(3,-2,1,1,2,0),3,2)
try(parse_cblv(x,0,3))
x <- matrix(c(3,2,1,-1,2,0),3,2)
try(parse_cblv(x,0,3))
x <- matrix(c(3,1,0.1,1,2.5,0),3,2)
try(parse_cblv(x,0,3))

## FIXME: not ladderized, yet returns result
x <- matrix(c(2,1,3,1,2,0),3,2)
all(cblv(parse_cblv(x,0,3))==x)

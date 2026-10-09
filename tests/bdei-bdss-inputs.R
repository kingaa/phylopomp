## Input domains must be checked before initializing a genealogy filter.
suppressPackageStartupMessages({
  library(phylopomp)
  library(pomp)
})

tree <- parse_newick("a:1;")
rejects <- function (expr, pattern) {
  msg <- tryCatch({force(expr); NULL}, error=conditionMessage)
  stopifnot(is.character(msg))
  if (!grepl(pattern,gsub("[[:space:]]+"," ",msg)))
    stop("Unexpected error: ",msg)
}

for (model in c("bdei","bdss")) {
  if (model == "bdei") {
    constructor <- "bdei_pomp"
    args <- list(x=tree,sigma=1,lambda=1,mu=0.2,chi=0.6,
      pop=1,E0=0,I0=1)
    initial <- c("E0","I0")
    birth <- "lambda"
  } else {
    constructor <- "bdss_pomp"
    args <- list(x=tree,lambda_nn=1,lambda_ns=0,lambda_sn=0,lambda_ss=0,
      mu=0.2,chi=0.6,pop=1,N0=1,S0=0)
    initial <- c("N0","S0")
    birth <- "lambda_nn"
  }

  po <- do.call(constructor,args)
  stopifnot(inherits(po,"pomp"),all(is.finite(rinit(po)[seq_len(6L),,drop=FALSE])))
  for (nm in setdiff(names(args),"x")) {
    for (bad in list(-1,NA_real_,NaN,Inf,c(1,2),numeric(0),"1")) {
      altered <- args
      altered[nm] <- list(bad)
      rejects(do.call(constructor,altered),"finite, nonnegative numeric scalars")
    }
  }
  altered <- args
  altered[initial] <- list(0,0)
  rejects(do.call(constructor,altered),"finite positive sum")
  altered[initial] <- list(.Machine$double.xmax,.Machine$double.xmax)
  rejects(do.call(constructor,altered),"finite positive sum")
  altered[initial] <- list(.Machine$double.xmin,0)
  altered$pop <- .Machine$double.xmax
  rejects(do.call(constructor,altered),"pop.*must be finite")

  ## POMP can replace coefficients without calling the constructor again.
  coef(po)[initial] <- 0
  rejects(rinit(po),"initial values")
  po <- do.call(constructor,args)
  coef(po)[birth] <- -1
  rejects(rinit(po),"rates must be finite and nonnegative")

  ## Zero rates and an empty initial population are legitimate boundaries.
  altered <- args
  altered$pop <- 0
  altered[birth] <- list(0)
  stopifnot(all(is.finite(rinit(do.call(constructor,altered))[seq_len(6L),,drop=FALSE])))
}

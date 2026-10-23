## Here we are attempting to verify that the three-strains filter,
## which has been edited to mimic the LBDP filter, does in fact
## return the same likelihoods.

library(tidyverse)
library(phylopomp)
library(circumstance)
library(foreach)
library(iterators)
library(doFuture)
plan(multicore)

## the full forest
readLines("B.1.617_phylopomp.nwk") |>
  parse_newick(t0=0) |>
  getInfo(t0=T,time=T)

## subsample
readLines("B.1.617_phylopomp.nwk") |>
  parse_newick(t0=0) |>
  curtail(t0=268.67) |>
  subsample(frac=0.05) |>
  freeze(seed=331430581) -> t0

## split out the individual trees
t0 |>
  newick(obscure=FALSE) |>
  strsplit(";") |>
  getElement(1) |>
  paste0(";") -> n
length(n)

## number of tips in each deme
1:3 |>
  sprintf("deme=%d",x=_) |>
  lapply(\(p)grepl(pattern=p,x=n)) |>
  sapply(sum)

## extract deme-specific trees
1:3 |>
  sprintf("deme=%d",x=_) |>
  lapply(\(p)grep(pattern=p,x=n,val=TRUE)) |>
  lapply(paste0,collapse="") |>
  lapply(parse_newick,t0=268.67,time=355.03) -> t

t |>
  sapply(newick,obscure=TRUE,extended=FALSE) |>
  writeLines("strains.nwk")
  

## add individual trees back together
geneal_sum(t) -> t.all
## verify that this recovers the original forest
stopifnot(all.equal(cblv(t0), cblv(t.all)))

plot(t.all,obscure=FALSE)

## compute log likelihoods, maximized over n0
t |>
  sapply(
    lbdp_exact,
    lambda=0.2, mu=1/7, chi=0.0002999, psi=0, n0=NA
  ) -> ll1

## these are the estimated n0
n0 <- c(322,88,9)

## get the exact likelihoods for each strain-specific genealogy
1:3 |>
  sapply(
    \(k) lbdp_exact(t[[k]], lambda=0.2, mu=1/7, chi=0.0002999, psi=0, n0=n0[k])
  ) -> ll

## verify that the n0 given are correct
stopifnot(all.equal(ll,ll1))

## compute likelihood of full forest
t.all |>
  lbdp_exact(
    lambda=0.2, mu=1/7, chi=0.0002999, psi=0, n0=sum(n0)
  ) -> ll.all

## get deme-specific numbers of roots
t |>
  lapply(getInfo,nroot=TRUE) |>
  unlist() |>
  unname() -> nr

## The likelihoods agree up to the multinomial factor
## `( n0 ) / ( n0-nr )`
## Here, the multinomial coefficient `( n )` is defined for
## integer vector `n = (n1, n2, ..., nk)` by the formula
## `( n ) = (n1+...+nk)! / (n1! n2! ... nk!)`.
## 
## Thus `log(( n )) = lfactorial(sum(n))-sum(lfactorial(n))`
## where `lfactorial` is R's vectorized function
## `x -> log(factorial(x))`.

logmf <- function (n0, nr)
  lfactorial(sum(n0))-sum(lfactorial(n0))-
    lfactorial(sum(n0-nr))+sum(lfactorial(n0-nr))

stopifnot(all.equal(sum(ll)+logmf(n0,nr), ll.all))

## Compute likelihoods using LBDP filter
## First, treat each strain separately.
tibble(
  deme=sprintf("deme%d",1:3),
  genealogy=t,
  n0=n0,
  lambda=0.2,
  mu=1/7,
  chi=0.0002999,
  psi=0
) |>
  expand_grid(
    rep=seq.int(16)
  ) -> grid

foreach (r=iter(grid,"row")) %dofuture% {
  r |>
    with(
      genealogy[[1]] |>
        lbdp_pomp(lambda=lambda,mu=mu,chi=chi,psi=psi,n0=n0) |>
        pfilter(Np=1e3)
    ) -> pf
  bind_cols(r,tibble(pf=list(pf)))
} %seed% 139135522 |>
  bind_rows() |>
  bake(file="strains_linear1.rds") |>
  mutate(value=sapply(pf,logLik)) |>
  group_by(deme) |>
  reframe(
    name=c("est","se","ess"),
    sd=sd(value),
    value=logmeanexp(value,se=TRUE,ess=TRUE)
  ) |>
  pivot_wider() -> pfs1

pfs1

## Compute likelihoods using LBDP filter
## Next, filter whole genealogy.
tibble(
  genealogy=list(geneal_sum(t)),
  n0=sum(n0),
  lambda=0.2,
  mu=1/7,
  chi=0.0002999,
  psi=0
) |>
  expand_grid(
    rep=seq.int(16)
  ) -> grid

foreach (r=iter(grid,"row")) %dofuture% {
  r |>
    with(
      genealogy[[1]] |>
        lbdp_pomp(lambda=lambda,mu=mu,chi=chi,psi=psi,n0=n0) |>
        pfilter(Np=1e3)
    ) -> pf
  bind_cols(r,tibble(pf=list(pf)))
} %seed% 139135522 |>
  bind_rows() |>
  bake(file="strains_linear1a.rds") |>
  mutate(value=sapply(pf,logLik)) |>
  reframe(
    name=c("est","se","ess"),
    sd=sd(value),
    value=logmeanexp(value,se=TRUE,ess=TRUE)
  ) |>
  pivot_wider() -> pfs1a

pfs1a

## Compute likelihood under three-strain model.
## `strains_pomp.c` has been edited to make this effectively
## linear. Parameters are chosen to match assumptions above.
tibble(
  genealogy=list(geneal_sum(t)),
  pop=1e5,
  I1_0=n0[1]/pop,
  I2_0=n0[2]/pop,
  I3_0=n0[3]/pop,
  R_0=0,
  S_0=1-(I1_0+I2_0+I3_0),
  gamma=1/7,
  chi=0.0002999,
  psi=0,
  lambda=0.2,
  Beta1=lambda/S_0,
  Beta2=lambda/S_0,
  Beta3=lambda/S_0
) |>
  expand_grid(
    rep=seq.int(16)
  ) -> grid

foreach (r=iter(grid,"row")) %dofuture% {
  r |>
    with(
      genealogy[[1]] |>
        strains_pomp(
          Beta1=Beta1,Beta2=Beta2,Beta3=Beta3,
          gamma=gamma,chi=chi,pop=pop,
          S_0=S_0,I1_0=I1_0,I2_0=I2_0,I3_0=I3_0,R_0=R_0
        ) |>
        pfilter(Np=1e3)
    ) -> pf
  bind_cols(r,tibble(pf=list(pf)))
} %seed% 139135522 |>
  bind_rows() |>
  bake(file="strains_linear2.rds") |>
  mutate(value=sapply(pf,logLik)) |>
  reframe(
    name=c("est","se","ess"),
    sd=sd(value),
    value=logmeanexp(value,se=TRUE,ess=TRUE)
  ) |>
  pivot_wider() -> pfs2

pfs2
sum(pfs1$est)+logmf(n0,nr)

## Repeat using more particles and `wpfilter`.
foreach (r=iter(grid,"row")) %dofuture% {
  r |>
    with(
      genealogy[[1]] |>
        strains_pomp(
          Beta1=Beta1,Beta2=Beta2,Beta3=Beta3,
          gamma=gamma,chi=chi,pop=pop,
          S_0=S_0,I1_0=I1_0,I2_0=I2_0,I3_0=I3_0,R_0=R_0
        ) |>
        wpfilter(Np=1e4,trigger=0.1,target=0.5)
    ) -> pf
  bind_cols(r,tibble(pf=list(pf)))
} %seed% 139135522 |>
  bind_rows() |>
  bake(file="strains_linear3.rds") |>
  mutate(value=sapply(pf,logLik)) |>
  reframe(
    name=c("est","se","ess"),
    sd=sd(value),
    value=logmeanexp(value,se=TRUE,ess=TRUE)
  ) |>
  pivot_wider() -> pfs3

pfs3
sum(pfs1$est)+logmf(n0,nr)

## Treat each strain-specific genealogy independently.
tibble(
  deme=sprintf("deme%d",1:3),
  genealogy=t,
  pop=1e5,
  I1_0=n0[1]/pop,
  I2_0=n0[2]/pop,
  I3_0=n0[3]/pop,
  R_0=0,
  S_0=1-(I1_0+I2_0+I3_0),
  gamma=1/7,
  chi=0.0002999,
  psi=0,
  lambda=0.2,
  Beta1=lambda/S_0,
  Beta2=lambda/S_0,
  Beta3=lambda/S_0
) |>
  expand_grid(
    rep=seq.int(16)
  ) -> grid

foreach (r=iter(grid,"row")) %dofuture% {
  r |>
    with(
      genealogy[[1]] |>
        strains_pomp(
          Beta1=Beta1,Beta2=Beta2,Beta3=Beta3,
          gamma=gamma,chi=chi,pop=pop,
          S_0=S_0,I1_0=I1_0,I2_0=I2_0,I3_0=I3_0,R_0=R_0
        ) |>
        wpfilter(Np=1e3,trigger=0.1,target=0.2)
    ) -> pf
  bind_cols(r,tibble(pf=list(pf)))
} %seed% 139135522 |>
  bind_rows() |>
  bake(file="strains_linear4.rds") |>
  mutate(value=sapply(pf,logLik)) |>
  group_by(deme) |>
  reframe(
    name=c("est","se","ess"),
    sd=sd(value),
    value=logmeanexp(value,se=TRUE,ess=TRUE)
  ) |>
  pivot_wider() -> pfs4

pfs4
sum(pfs1$est)+logmf(n0,nr)

################################


## readLines("B.1.617_short_20_Percent_Retained_phylopomp.nwk") |>
##   parse_newick(t0=0) -> g
## g |> plot(obscure=FALSE)

## g |>
##   lbdp_exact(
##     lambda=0.2,
##     mu=1/7,
##     chi=0.0002999,
##     psi=0,
##     n0=1358+434+7,
##     )

## g |>
##   lbdp_pomp(
##     lambda=0.2,
##     mu=1/7,
##     chi=0.0002999,
##     psi=0,
##     n0=1358+434+7
##   ) |>
##   pfilter(Np=1000,Nrep=20) |>
##   logLik() |>
##   logmeanexp(se=TRUE,ess=TRUE)

## factor <- 1e6/898381

## g |>
##   strains_pomp(
##     Beta1=0.2*factor,
##     Beta2=0.2*factor,
##     Beta3=0.2*factor,
##     gamma=1/7,
##     chi=0.0002999,
##     pop=1e6,
##     S_0=0.9,
##     I1_0=1.36e-3,
##     I2_0=4.35e-4,
##     I3_0=7e-6,
##     R_0=0.1
##   ) |> pfilter(Np=1000,Nrep=20) -> pfs

## logLik(pfs) |>
##   logmeanexp(se=TRUE,ess=TRUE)

## ################################

## runLBDP(time=3,lambda=3,mu=1,chi=0.2,psi=0,n0=1) |>
##   freeze(seed=1076955200) -> x
## x |> plot()

## x |>
##   lbdp_exact(
##     lambda=3,mu=1,chi=0.2,psi=0,n0=1
##   )

## x |>
##   lbdp_pomp(
##     lambda=3,mu=1,chi=0.2,psi=0,n0=1
##   ) |>
##   pfilter(Np=1000,Nrep=10) |>
##   logLik() |>
##   logmeanexp(se=TRUE,ess=TRUE)

## runLBDP(time=90,lambda=0.2,mu=1/7,chi=0.0003,psi=0,n0=1800) |>
##   freeze(seed=1076955200) -> x
## x |> plot()

## x |>
##   lbdp_exact(
##     lambda=3,mu=1,chi=0.2,psi=0,n0=50
##   )

## x |>
##   lbdp_pomp(
##     lambda=3,mu=1,chi=0.2,psi=0,n0=50
##   ) |>
##   pfilter(Np=1000,Nrep=10) |>
##   logLik() |>
##   logmeanexp(se=TRUE,ess=TRUE)

## g |>
##   lineages() |>
##   mutate(dt=time-lag(time)) |>
##   slice(-1) |>
##   filter(dt==0)

## filter(dt<0.000001)


## readLines("B.1.617_short_20_Percent_Retained_phylopomp.nwk") |>
##   parse_newick(t0=0) |>
##   strains_pomp(
##     Beta1=0.1856,
##     Beta2=0.1875,
##     Beta3=0.2181,
##     gamma=1/7,
##     chi=0.0002999,
##     pop=1e6,
##     S_0=0.9,
##     I1_0=1.36e-3,
##     I2_0=4.35e-4,
##     I3_0=7e-6,
##     R_0=0.1
##   ) |> pfilter(Np=1000,Nrep=20) -> pfs

## logLik(pfs) |>
##   logmeanexp(se=TRUE,ess=TRUE)

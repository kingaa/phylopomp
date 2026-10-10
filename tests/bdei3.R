options(digits=3)
suppressPackageStartupMessages({
  library(pomp)
  library(phylopomp)
})

## lambda=0, one founding infectious, destructive sampling only:
## BDEI (any sigma) is exactly LBDP, so bdei_pomp must recover lbdp_exact
## with no Monte Carlo error.
freeze(
  runLBDP(time=1.5, lambda=0, mu=0.2, chi=2, psi=0, n0=1),
  seed=1
) -> t0
stopifnot(getInfo(t0, nsample=TRUE)$nsample >= 1)
ll_exact0 <- lbdp_exact(t0, lambda=0, mu=0.2, psi=0, chi=2, n0=1)
po0 <- bdei_pomp(
  t0, sigma=50, lambda=0, mu=0.2, chi=2, pop=1, E0=0, I0=1
)
ll_bdei0 <- replicate(5, logLik(pfilter(po0, Np=20)))
stopifnot(
  is.finite(ll_exact0),
  all(is.finite(ll_bdei0)),
  all(abs(ll_bdei0 - ll_exact0) < 1e-8)
)

## sigma -> infinity: an LBDP tree (psi=0, chi>0) evaluated under BDEI
## with large sigma must recover lbdp_exact to within Monte Carlo error.
## (Inline E->I nodes are erased by obscuring; the newborn E must progress
## before the next I-only event, which a large sigma makes immediate.)
freeze(
  runLBDP(time=3, lambda=1, mu=0.3, chi=0.3, psi=0, n0=2),
  seed=2718
) -> tree
ll_exact <- lbdp_exact(tree, lambda=1, mu=0.3, psi=0, chi=0.3, n0=2)
po <- bdei_pomp(
  tree, sigma=1000, lambda=1, mu=0.3, chi=0.3, pop=2, E0=0, I0=2
)
replicate(8, logLik(pfilter(po, Np=4000))) |>
  logmeanexp(se=TRUE) -> pf_ll
stopifnot(
  is.finite(ll_exact),
  is.finite(pf_ll[1]),
  pf_ll[1] > ll_exact - 3*pf_ll[2],
  pf_ll[1] < ll_exact + 3*pf_ll[2]
)

## Exposed founder, lambda=0, and one destructive sample at t=1.
## The root is at t=0. An E founder must progress at rate sigma before the
## tip can be sampled, and after progression must avoid removal and sampling
## for the remaining time. Integrating over its progression time gives:
##   L = chi*sigma*(exp(-(mu+chi)*t)-exp(-sigma*t))/(sigma-(mu+chi)).
## This is an independent oracle for the E -> I transition and its weighting.
t <- 1
sigma <- 1.2
mu <- 0.3
chi <- 0.4
ll_exposed_exact <- log(
  chi*sigma*(exp(-(mu+chi)*t)-exp(-sigma*t))/(sigma-(mu+chi))
)
exposed_tree <- parse_newick("a:1;")
po_exposed <- bdei_pomp(
  exposed_tree, sigma=sigma, lambda=0, mu=mu, chi=chi,
  pop=1, E0=1, I0=0
)
set.seed(314159)
exposed_ll <- replicate(20, logLik(pfilter(po_exposed, Np=5000)))
exposed_lr <- exp(exposed_ll - ll_exposed_exact)
exposed_lr_mean <- mean(exposed_lr)
exposed_lr_se <- sd(exposed_lr)/sqrt(length(exposed_lr))
stopifnot(
  is.finite(ll_exposed_exact),
  all(is.finite(exposed_ll)),
  abs(exposed_lr_mean - 1) < 3*exposed_lr_se
)

## Two exposed founders exercise the progression weight when the tracked
## lineage enters a population with I=2. With mu=lambda=0, one observed
## sample can arise from either host; the other host must remain unsampled.
## For one host, f_E(t) is the density above with mu=0. The other-host
## no-sample probability is
##   S_E(t)=exp(-sigma*t)+sigma*(exp(-chi*t)-exp(-sigma*t))/(sigma-chi),
## so the one-tip likelihood is 2*f_E(t)*S_E(t).
f_E <- chi*sigma*(exp(-chi*t)-exp(-sigma*t))/(sigma-chi)
S_E <- exp(-sigma*t)+sigma*(exp(-chi*t)-exp(-sigma*t))/(sigma-chi)
ll_two_exposed_exact <- log(2*f_E*S_E)
po_two_exposed <- bdei_pomp(
  exposed_tree, sigma=sigma, lambda=0, mu=0, chi=chi,
  pop=2, E0=2, I0=0
)
two_exposed_ll <- replicate(20, logLik(pfilter(po_two_exposed, Np=5000)))
two_exposed_lr <- exp(two_exposed_ll - ll_two_exposed_exact)
two_exposed_lr_mean <- mean(two_exposed_lr)
two_exposed_lr_se <- sd(two_exposed_lr)/sqrt(length(two_exposed_lr))
stopifnot(
  is.finite(ll_two_exposed_exact),
  all(is.finite(two_exposed_ll)),
  abs(two_exposed_lr_mean - 1) < 3*two_exposed_lr_se
)

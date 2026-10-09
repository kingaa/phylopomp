options(digits=3)
suppressPackageStartupMessages({
  library(pomp)
  library(phylopomp)
})

## With the same seed, BDSS restricted to normal spreaders
## (lambda_ns=lambda_sn=lambda_ss=0, S0=0) should draw the same nsample
## as runLBDP with matched (lambda_nn, mu, chi, n0).
set.seed(2718)
a <- runBDSS(
  time=6, pop=3, N0=3, S0=0,
  lambda_nn=1, lambda_ns=0, lambda_sn=0, lambda_ss=0,
  mu=0.3, chi=0.2
)
set.seed(2718)
b <- runLBDP(time=6, lambda=1, mu=0.3, chi=0.2, psi=0, n0=3)
stopifnot(
  identical(
    getInfo(a, nsample=TRUE)$nsample,
    getInfo(b, nsample=TRUE)$nsample
  )
)

## Exact-likelihood pfilter bridge: bdss_pomp() + pfilter() should recover
## lbdp_exact to within Monte Carlo error on that LBDP-reduced tree.
po <- bdss_pomp(
  a,
  lambda_nn=1, lambda_ns=0, lambda_sn=0, lambda_ss=0,
  mu=0.3, chi=0.2,
  pop=3, N0=3, S0=0
)
ll_exact <- lbdp_exact(a, lambda=1, mu=0.3, psi=0, chi=0.2, n0=3)
replicate(8, logLik(pfilter(po, Np=2000))) |>
  logmeanexp(se=TRUE) -> pf_ll
stopifnot(
  is.finite(ll_exact),
  is.finite(pf_ll[1]),
  pf_ll[1] > ll_exact - 3*pf_ll[2],
  pf_ll[1] < ll_exact + 3*pf_ll[2]
)

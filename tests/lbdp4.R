options(digits=3)
suppressPackageStartupMessages({
  library(pomp)
  library(phylopomp)
})
set.seed(1253219857)

runLBDP(time=1,lambda=0,mu=0,psi=10,r=1,n0=5) |>
  gendat() -> gi
sample_idx <- gi$nodetype == 1L
stopifnot(
  `no samples`=any(sample_idx),
  `bad saturation`=all(gi$saturation[sample_idx] == 0)
)

runLBDP(time=1,lambda=0,mu=0,psi=100,r=0,n0=5) |>
  gendat() -> gi
sample_idx <- gi$nodetype == 1L
stopifnot(
  `no samples`=any(sample_idx),
  `bad saturation`=sum(gi$saturation[sample_idx] == 0)==5
)

try(runLBDP(time=1,lambda=0,mu=0,psi=1,r=-0.1,n0=1))
try(runLBDP(time=1,lambda=0,mu=0,psi=1,r=1.5,n0=1))
try(runLBDP(time=1,lambda=0,mu=0,psi=1,r=Inf,n0=1))
try(runLBDP(time=1,lambda=0,mu=0,psi=1,r=NA,n0=1))

freeze(seed=1772991206,
  runLBDP(time=2,lambda=2,mu=1,psi=1,r=0.4,n0=10)
) -> x

stopifnot(
  `lbdp_exact supports r in (0,1)`=is.finite(
    x |> lbdp_exact(lambda=2,mu=1,psi=1,r=0.4,n0=10)
  ),
  `lbdp_pomp supports r>0`=is.finite(
    x |>
      lbdp_pomp(lambda=2,mu=1,psi=1,r=0.4,n0=10) |>
      pfilter(Np=10000) |>
      logLik()
  )
)

## Calibration: exact likelihood and pfilter should agree for BDD(r) across r in {0, 1} and interior
lambda <- 2; mu <- 1; psi <- 1; n0 <- 10L; time <- 1.5
tol_log <- 1.5   ## allow pfilter to be within tol_log of exact (Monte Carlo error)
Np <- 8000
for (r_val in c(0, 0.5, 1)) {
  set.seed(90125 + round(r_val * 100))
  tree <- runLBDP(time=time, lambda=lambda, mu=mu, psi=psi, r=r_val, n0=n0)
  exact_ll <- lbdp_exact(tree, lambda=lambda, mu=mu, psi=psi, r=r_val, n0=n0)
  set.seed(90225 + round(r_val * 100))
  pf_ll <- tree |>
    lbdp_pomp(lambda=lambda, mu=mu, psi=psi, r=r_val, n0=n0) |>
    pfilter(Np=Np) |>
    logLik()
  stopifnot(
    is.finite(exact_ll),
    is.finite(pf_ll),
    pf_ll >= exact_ll - tol_log,
    pf_ll <= exact_ll + tol_log
  )
}

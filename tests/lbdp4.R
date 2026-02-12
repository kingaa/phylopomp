options(digits=3)
suppressPackageStartupMessages({
  library(pomp)
  library(phylopomp)
})
set.seed(1253219857)

runLBDP(time=1,lambda=0,mu=0,psi=0,chi=10,n0=5) |>
  gendat() -> gi
sample_idx <- gi$nodetype == 1L
stopifnot(
  `no samples`=any(sample_idx),
  `bad saturation`=all(gi$saturation[sample_idx] == 0)
)

runLBDP(time=1,lambda=0,mu=0,psi=100,chi=0,n0=5) |>
  gendat() -> gi
sample_idx <- gi$nodetype == 1L
stopifnot(
  `no samples`=any(sample_idx),
  `bad saturation`=sum(gi$saturation[sample_idx] == 0)==5
)

freeze(seed=1772991206,
  runLBDP(time=2,lambda=2,mu=1,psi=0.6,chi=0.4,n0=10)
) -> x

stopifnot(
  `lbdp_exact supports chi in (0,1)`=is.finite(
    x |> lbdp_exact(lambda=2,mu=1,psi=0.6,chi=0.4,n0=10)
  ),
  `lbdp_pomp supports chi>0`=is.finite(
    x |>
      lbdp_pomp(lambda=2,mu=1,psi=0.6,chi=0.4,n0=10) |>
      pfilter(Np=10000) |>
      logLik()
  )
)

## Calibration: exact likelihood and pfilter should agree
for (r in c(0,0.23,0.5,0.89,1)) {
  tree <- runLBDP(time=2,lambda=2,mu=1,psi=1-r,chi=r,n0=10)
  exact_ll <- lbdp_exact(tree,lambda=2,mu=1,psi=1-r,chi=r,n0=10)
  tree |>
    lbdp_pomp(lambda=2,mu=1,psi=1-r,chi=r,n0=10) |>
    pfilter(Np=2000) |>
    logLik() |>
    replicate(n=10) |>
    logmeanexp(se=TRUE) -> pf_ll
  stopifnot(
    is.finite(exact_ll),
    is.finite(pf_ll),
    pf_ll[1] > exact_ll-3*pf_ll[2],
    pf_ll[1] < exact_ll+3*pf_ll[2]
  )
}

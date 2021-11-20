suppressPackageStartupMessages({
  library(phylopomp)
  library(pomp)
  library(tidyverse)
  library(broom)
  library(doParallel)
  library(doRNG)
})

options(digits=3)
png(filename="lbdp3-%02d.png",res=100)
theme_set(theme_bw())

chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
if (nzchar(chk) && chk == "TRUE") {
  ## use 2 cores in CRAN/Travis/AppVeyor
  registerDoParallel(2)
} else {
  ## use all cores in devtools::test()
  registerDoParallel()
}
registerDoRNG(728604304)

playLBDP(lambda=4,mu=2,psi=4,times=5) -> x
x |> getInfo(compact=TRUE) |> plot(points=TRUE)
x |> getInfo() |> getElement("tree") |>
  newick2df(time=5) -> dat

dat |> lbdp_exact(lambda=4,mu=3,psi=4) -> ll1

dat |>
  lbdp_pomp(lambda=4,mu=3,psi=4) |>
  pfilter(
    Np=1000
  ) |>
  logLik() -> ll2

stopifnot(all.equal(ll1,ll2,tol=0.3))

dev.off()

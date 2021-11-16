suppressPackageStartupMessages({
  library(phylopomp)
  library(pomp)
  library(tidyverse)
  library(broom)
  library(doParallel)
  library(doRNG)
})
options(digits=3)
png(filename="sirs1-%02d.png",res=100)
theme_set(theme_bw())
set.seed(328168304L)

chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
if (nzchar(chk) && chk == "TRUE") {
  ## use 2 cores in CRAN/Travis/AppVeyor
  registerDoParallel(2)
} else {
  ## use all cores in devtools::test()
  registerDoParallel()
}
registerDoRNG(328168304L)

playSIRS(
  Beta=4,gamma=2,psi=1,Delta=1,
  S0=97,I0=3,R0=0,t0=0,times=40,
  tree=TRUE
) -> x

x |>
  getInfo() |>
  getElement("tree") |>
  newick2df(time=40) -> sirs_dat

sirs_dat |>
  sirs_pomp(
    Beta=4,gamma=2,psi=1,Delta=1,
    S0=97,I0=3,R0=0,t0=0
  ) |>
  pfilter(Np=1000) |>
  logLik()

x |>
  getInfo(compact=TRUE) |>
  plot(points=FALSE) |>
  getElement(1L)

dev.off()

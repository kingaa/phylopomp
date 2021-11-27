suppressPackageStartupMessages({
  library(tidyverse)
  library(magrittr)
  library(ggtree)
  library(pomp)
  library(phylopomp)
  library(cowplot)
  library(doParallel)
  library(doRNG)
})

options(digits=3)
png(filename="lbdp4-%02d.png",res=100)
theme_set(theme_bw())

expand_grid(
  rep=1:8,
  lambda=2,
  mu=0.5,
  psi=5,
  n0=1,
  times=1,
  Np=c(1000,10000,100000)
) -> params

freeze(
  params[1,] %$% playLBDP(lambda=lambda,mu=mu,psi=psi,n0=n0,times=times),
  seed=813200894
) -> x

x |>
  getInfo() |>
  getElement("tree") |>
  newick2df(time=5) -> lbdp_dat

params[1,] %$% {
  lbdp_dat |>
    lbdp_exact(lambda=lambda,mu=mu,psi=psi)
}

chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
if (nzchar(chk) && chk == "TRUE") {
  ## use 2 cores in CRAN/Travis/AppVeyor
  registerDoParallel(2)
} else {
  ## use all cores in devtools::test()
  registerDoParallel()
}
registerDoRNG(453362294)

foreach (
  p=iter(params,"row"),
  .inorder=FALSE,
  .packages=c("magrittr","phylopomp","pomp"),
  .combine=bind_rows
) %dopar% {
  p %$% {
    lbdp_dat |>
      lbdp_exact(lambda=lambda,mu=mu,psi=psi)
  } -> ll1
  p %$% {
    lbdp_dat |>
      lbdp_pomp(lambda=lambda,mu=mu,psi=psi,n0=n0) |>
      pfilter(Np=Np) |>
      logLik()
  } -> ll2
  bind_cols(p,exact=ll1,pf=ll2)
} -> params

x |>
  getInfo(compact=TRUE) |>
  plot(points=TRUE) |>
  getElement(1L) -> pl1

params |>
  pivot_longer(c(exact,pf)) |>
  unite(name,name,Np) |>
  mutate(
    name=if_else(grepl("exact",name),"exact",name),
    name=gsub("pf_","",name)
  ) |>
  group_by(lambda,mu,psi,times,n0,name) |>
  summarize(
    type=c("logLik","logLik_se"),
    value=logmeanexp(value,se=TRUE)
  ) |>
  ungroup() |>
  pivot_wider(names_from=type) |>
  mutate(
    y=logLik,
    ymax=logLik+2*logLik_se,
    ymin=logLik-2*logLik_se
  ) -> params

params |>
  ggplot(aes(x=lambda,group=name,color=name,
    y=y,ymin=ymin,ymax=ymax))+
  geom_errorbar(
    position="dodge"
  )+
  scale_color_manual(
    labels=c(
      exact="exact",
      `1000`="1000 particles",
      `10000`="10000 particles",
      `1e+05`="100000 particles"
    ),
    values=c(
      exact="black",
      `1000`="blue",
      `10000`="red",
      `1e+05`="green"
    )
  )+
  labs(
    color=character(0),
    y="log likelihood",
    x=expression(lambda)
  )+
  theme(
    legend.position="bottom"
  ) -> pl2

plot_grid(
  A=pl1,
  B=pl2,
  labels="AUTO",
  nrow=1,
  rel_widths=c(1,1)
)

dev.off()

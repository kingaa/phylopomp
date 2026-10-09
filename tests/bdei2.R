png(filename="bdei2-%02d.png",res=100)

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(tidyverse)
  library(pomp)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(20260817)

freeze(
  simulate(
    "BDEI",
    time=2.5, sigma=1, lambda=1.5, mu=0.3, chi=0.6,
    pop=3, E0=0, I0=3
  ),
  seed=20260817
) -> G
G |> plot(obscure=FALSE,points=TRUE)

stopifnot(
  attr(G,"model")=="BDEI",
  getInfo(G,nsample=TRUE)$nsample>=2
)

try(
  G |>
    bdei_pomp(
      sigma=1, lambda=1.5, mu=0.3, chi=0.6,
      pop=3, E0=-1, I0=3
    ) -> po
)

G |>
  bdei_pomp(
    sigma=1, lambda=1.5, mu=0.3, chi=0.6,
    pop=3, E0=0, I0=3
  ) -> po

po |> rinit(nsim=5)

po |> pfilter(Np=1) |> cond_logLik()
po |> pfilter(Np=1000) |> replicate(n=10) |> concat() -> pf
pf[[1]] |> cond_logLik()
pf |> logLik()
pf |> logLik() |> logmeanexp(se=TRUE,ess=TRUE)

stopifnot(
  all(is.finite(logLik(pf))),
  all(logLik(pf) > -Inf)
)

plot_grid(
  G |>
    plot(points=TRUE)+
    expand_limits(x=2.5),
  pf |>
    cond_logLik(format="d") |>
    ggplot(aes(x=time,y=cond.logLik,group=.id))+
    geom_step(direction="vh",alpha=0.3)+
    labs(x="")+
    expand_limits(x=2.5),
  pf |>
    eff_sample_size(format="d") |>
    ggplot(aes(x=time,y=eff.sample.size,group=.id))+
    geom_step(direction="vh",alpha=0.3)+
    geom_hline(yintercept=100,color="red")+
    expand_limits(x=2.5),
  ncol=1,align="v",rel_heights=c(2,1,1)
)

dev.off()

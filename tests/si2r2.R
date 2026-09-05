png(filename="si2r2-%02d.png",res=100)

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(tidyverse)
  library(pomp)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(802530120)

simulate(
  "SI2R",time=3,
  omega=2,etaL=0.5,kappa=10,pop=100,
  Beta=5,gamma=1,chi=1,etaH=3,
  S0=0.95,IL0=0.03,IH0=0.02,R0=0
) -> G
G |> plot(obscure=FALSE,points=TRUE)

try(
  G |>
    si2rs_pomp(
      omega=2,etaL=0.5,kappa=10,pop=100,
      Beta=5,gamma=1,chi=1,etaH=3,
      S0=0.98,IL0=-0.02,IH0=0,R0=0
    )
)

G |>
  curtail(time=1) |>
  si2rs_pomp(
    omega=2,etaL=0.5,kappa=10,pop=100,
    Beta=5,gamma=1,chi=1,etaH=3,
    S0=0.95,IL0=0.03,IH0=0.02,R0=0
  ) -> po

po |>
  rinit(nsim=5) |>
  melt() -> ri
stopifnot(
  ri |> filter(name=="S",value==95) |> nrow()==5,
  ri |> filter(name=="IL",value==3) |> nrow()==5,
  ri |> filter(name=="IH",value!=2) |> nrow()==0
)

po |> pfilter(Np=1) |> cond_logLik()
po |> pfilter(Np=1000) |> replicate(n=10) |> concat() -> pf
pf[[1]] |> cond_logLik()
pf |> logLik()
pf |> logLik() |> logmeanexp(se=TRUE,ess=TRUE)

plot_grid(
  G |>
    curtail(time=1) |>
    plot(points=TRUE)+
    expand_limits(x=1),
  pf |>
    cond_logLik(format="d") |>
    ggplot(aes(x=time,y=cond.logLik,group=.id))+
    geom_step(direction="vh",alpha=0.3)+
    labs(x="")+
    expand_limits(x=1),
  pf |>
    eff_sample_size(format="d") |>
    ggplot(aes(x=time,y=eff.sample.size,group=.id))+
    geom_step(direction="vh",alpha=0.3)+
    geom_hline(yintercept=100,color="red")+
    expand_limits(x=1),
  ncol=1,align="v",rel_heights=c(2,1,1)
)

po1 <- po
coef(po1,c("IL0","IH0")) <- 0
stopifnot(
  po1 |> pfilter(Np=100) |> logLik()==-Inf
)

dev.off()

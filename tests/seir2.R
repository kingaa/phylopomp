png(filename="seir2-%02d.png",res=100)

suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
  library(pomp)
})
theme_set(theme_bw())
set.seed(842110120)
options(digits=3)

runSEIR(
  Beta=4,sigma=1,gamma=1,psi=1,omega=1,
  S0=100,E0=3,I0=5,R0=100,
  time=5
) -> G
G |> plot(prune=FALSE,obscure=FALSE,points=TRUE)

G |> plot(points=TRUE)

G |>
  curtail(time=3) |>
  plot(points=TRUE)

try(
  G |>
    seirs_pomp(
      Beta=4,sigma=1,gamma=1,psi=1,omega=1,
      S0=100,E0=3,I0=-5,R0=100
    ) -> po
)

G |>
  curtail(time=3) |>
  seirs_pomp(
    Beta=4,sigma=1,gamma=1,psi=1,omega=1,
    S0=100,E0=3,I0=5,R0=100
  ) -> po

po |> rinit(nsim=5)

po |> pfilter(Np=1000) |> replicate(n=20) |> concat() -> pf
pf |> logLik()
pf |> logLik() |> logmeanexp(se=TRUE,ess=TRUE)

pf |> plot(type="s")

plot_grid(
  G |>
    curtail(time=3) |>
    plot(points=TRUE)+
    expand_limits(x=3),
  pf |>
    cond_logLik(format="d") |>
    ggplot(aes(x=time,y=cond.logLik,group=.id))+
    geom_step(direction="vh",alpha=0.3)+
    labs(x="")+
    expand_limits(x=3),
  pf |>
    eff_sample_size(format="d") |>
    ggplot(aes(x=time,y=eff.sample.size,group=.id))+
    geom_step(direction="vh",alpha=0.3)+
    geom_hline(yintercept=100,color="red")+
    expand_limits(x=3),
  ncol=1,align="v",rel_heights=c(2,1,1)
)

dev.off()

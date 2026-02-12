#' ---
#' title: Exact likelihood formula for the linear birth-death process with destructive sampling
#' author: A.A. King
#' output: rmarkdown::html_vignette
#' bibliography: phylopomp.bib
#' nocite: |
#'   @King2024, @King2022, @Stadler2010, @Gavryushkina2014
#' csl: jss.csl
#' vignette: >
#'   %\VignetteIndexEntry{Exact likelihood for LBDP with destructive sampling}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---
#' 

#' 
## ----packages-----------------------------------------------------------------
#| cache: false
#| include: false
library(tidyverse)
library(pomp)
library(phylopomp)
library(cowplot)
stopifnot(getRversion() >= "4.5")
stopifnot(packageVersion("pomp")>="6.2")
stopifnot(packageVersion("phylopomp")>="0.16.2")
theme_set(theme_bw(base_family="serif"))
options(
  dplyr.summarise.inform=FALSE,
  pomp_archive_dir="results/lbdp_destruc"
)
set.seed(1159254136)

#' 
#' The following codes simulate a realization of the genealogy process induced by a linear birth-death-sampling process with birth rate $\lambda$, death rate $\mu$, non-destructive sampling rate $\psi$, and destructive sampling rate $\chi$.
#' 
#' 
## ----lbdp1--------------------------------------------------------------------
true.params <- data.frame(lambda=3.5,mu=0.8,psi=1,chi=2,n0=20)
true.params |>
  with(
    runLBDP(lambda=lambda,mu=mu,psi=psi,chi=chi,n0=n0,time=3)
  ) -> x

#' 
## ----lbdp1_plot---------------------------------------------------------------
#| echo: false
plot_grid(
  x |> plot(points=TRUE),
  x |> lineages() |> plot(),
  ncol=1,
  align="hv",
  rel_heights=c(2,1)
)

#' 
#' The following designs a slice through parameter space in the direction of the parameter $r=\chi/(\psi+\chi)$, which is the fraction of the samples that are destructive.
#' 
## ----lbdp2--------------------------------------------------------------------
expand_grid(
  lambda=3.5,
  mu=0.8,
  r=seq(0,1,by=0.01),
  totsamp=with(true.params,psi+chi),
  n0=20
  ) |>
  mutate(
    psi=totsamp*(1-r),
    chi=totsamp*r
  ) -> params

#' 
#' Next, we perform a sequential Monte Carlo (particle filter) computation at each point in the above design.
#' 
## ----lbdp3--------------------------------------------------------------------
library(doParallel)
library(iterators)
foreach (
  p=iter(params,"row"),
  .inorder=FALSE,
  .combine=bind_rows
) %dopar% {
  p |>
    with(
      x |>
        lbdp_exact(lambda=lambda,mu=mu,psi=psi,chi=chi,n0=n0)
    ) -> ll1
  bind_cols(p,exact=ll1)
} |>
  bake(file="slice1.rds") -> params

#' 
## ----lbdp3_plot---------------------------------------------------------------
#| echo: false
params |>
  filter(is.finite(exact)) |>
  ggplot(aes(x=r,y=exact))+
  geom_line()+
  geom_vline(xintercept=with(true.params,chi/(psi+chi)),linetype=2)+
  labs(
    color=character(0),
    y="log likelihood",
    x=expression(r)
  )+
  theme(
    legend.position="inside",
    legend.position.inside=c(0.5,0.22),
    legend.background=element_rect(fill="white")
  )

#' 
#' ## References

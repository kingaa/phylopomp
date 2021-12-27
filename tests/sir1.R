png(filename="sir1-%02d.png",res=100)
suppressPackageStartupMessages({
  library(phylopomp)
  library(tidyverse)
})
theme_set(theme_bw())
set.seed(847110120)
options(digits=3)

runSIR(time=10) -> x
x

runSIR(Beta=3,gamma=1,psi=5,S0=100,I0=5,time=2) |>
  plot(points=TRUE)

runSIR(Beta=3,gamma=1,psi=2,S0=100,I0=5,time=1) |>
  simulate(time=100) |>
  plot(points=TRUE,time=NULL)

simulate("SIR",delta=1,time=20,I0=4) |>
  lineages() |>
  ggplot(aes(x=time,y=lineages))+
  geom_step()

dev.off()

library(phylopomp)
set.seed(147429846)

png(filename="diagram-01.png",res=100,
  width=874,height=198,units="px")
freeze(simulate("SIR",time=1),seed=147469846) |> diagram()
dev.off()

png(filename="diagram-02.png",res=100,
  width=874,height=198,units="px")
freeze(
  simulate("SIIR",time=0.3,psi1=1,psi2=1,S0=100),
  seed=788520677
) |> diagram()
dev.off()

png(filename="diagram-03.png",res=100,
  width=1277,height=134,units="px")
freeze(
  simulate("SIIR",time=0.3,psi1=1,psi2=1,S0=100),
  seed=459939038
) |> diagram(prune=FALSE)
dev.off()

png(filename="diagram-04.png",res=100,
  width=1338,height=134,units="px")
freeze(
  simulate("SIIR",time=0.3,psi1=1,psi2=1,S0=100),
  seed=459939038
) |> diagram(prune=FALSE,obscure=FALSE)
dev.off()

simulate("SIIR",time=0.3,sigma12=1,sigma21=0.1,delta=1) |>
  getInfo(description=TRUE) |>
  getElement("description") |>
  cat()

simulate("SIIR",time=0.3,sigma12=0.1,sigma21=1,delta=1) |>
  getInfo(prune=FALSE,obscure=FALSE,description=TRUE) |>
  getElement("description") |>
  cat()

simulate("SIR",time=0.5) |>
  getInfo(yaml=TRUE,t0=TRUE,prune=FALSE) |>
  getElement("yaml") |>
  cat()

simulate("LBDP",time=0.5) |>
  getInfo(yaml=TRUE) |>
  getElement("yaml") |>
  cat()

try(
  simulate("SIR",time=1) |>
    simulate(time=0.1)
)

pal <- c("#00274c55","#ffcb0555","#00659755")

png(filename="diagram-05.png",res=100,
  width=1000,height=190,units="px")
freeze(simulate("SEIR",time=1,delta=1),seed=234551276) |>
  diagram(obscure=FALSE,palette=pal)
dev.off()

try(
  simulate("SEIR",time=2,delta=1) |>
    diagram(obscure=FALSE,palette=pal[1])
)

simulate("SEIR",Beta=10,psi=2,sigma=3,time=1,delta=1) |>
  freeze(seed=118551276) -> x

png(filename="diagram-06.png",res=100,
  width=760,height=160,units="px")
x |>
  curtail(time=0.5) |>
  diagram(obscure=FALSE,palette=pal[c(6,7)])
dev.off()

library(grid)
library(scales)
library(dplyr)

png(filename="diagram-07.png",res=100,
  width=8,height=5,units="in")
plot_grid(
  x |>
    plot(obscure=FALSE,palette=alpha(pal,1)),
  x |>
    lineages(obscure=FALSE) |>
    mutate(deme=c("E","I")[deme]) |>
    plot(legend.position=c(0.2,0.9),palette=alpha(pal,1)),
  nrow=1,align="hv",axis="tblr",rel_widths=c(2,3)
) |>
  print(vp=viewport(height=0.8,y=0.6))
pushViewport(
  viewport(x=0.52,y=0.15,width=0.9,height=0.05,name="inset")
)
x |> diagram(obscure=FALSE,palette=pal) |> print(vp="inset")
x |>
  diagram(obscure=FALSE,palette=pal[c(2,3)]) |>
  print(vp=viewport(x=0.52,y=0.1,width=0.9,height=0.05))
dev.off()

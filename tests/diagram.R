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

simulate("SIR",time=0.5) |> yaml()

simulate("LBDP",time=0.5) |> yaml()

simulate("LBDP",time=0.5) |> yaml()

simulate("LBDP",time=0.5) |> geneal() |> yaml()

simulate("LBDP",time=0.5) |> yaml()

try(
  simulate("SIR",time=1) |> simulate(time=0.1)
)

pal <- c(`1`="#00274c55",`2`="#ffcb0555")

png(filename="diagram-05.png",res=100,
  width=1000,height=190,units="px")
freeze(simulate("SEIR",time=1,omega=1),seed=234551276) |>
  diagram(obscure=FALSE,palette=pal)
dev.off()

try(
  simulate("SEIR",time=2,omega=1) |>
    diagram(obscure=FALSE,palette=pal[1])
)

simulate("SEIR",Beta=10,psi=2,sigma=3,time=1,omega=1) |>
  freeze(seed=118551276) -> x

png(filename="diagram-06.png",res=100,
  width=760,height=160,units="px")
x |>
  curtail(time=0.5) |>
  diagram(obscure=FALSE,palette=c(NA,NA,NA))
dev.off()

library(grid)
library(scales)
library(dplyr)

try({
  pal <- c(`0`="#33333355",`3`="#ffcb0555",`2`="#00274c55")
  x |>
    plot(obscure=FALSE,palette=alpha(pal,1),
      legend.position="none"
    )
})

pal <- c(`0`="#33333355",`1`="#ffcb0555",`2`="#00274c55")

png(filename="diagram-07.png",res=100,
  width=8,height=5,units="in")
plot_grid(
  x |>
    plot(obscure=FALSE,palette=alpha(pal,1),
      legend.position="none"
    ),
  x |>
    lineages(obscure=FALSE) |>
    filter(deme > 0) |>
    plot(legend.position=c(0.2,0.9),palette=alpha(pal[c(2,3)],1)),
  nrow=1,align="hv",axis="tblr",rel_widths=c(2,3)
) |>
  print(vp=viewport(height=0.8,y=0.6))
x |>
  diagram(obscure=FALSE,palette=pal) |>
  print(vp=viewport(x=0.52,y=0.15,width=0.9,height=0.05))
x |>
  diagram(obscure=FALSE,palette=rev(unname(pal[-1L]))) |>
  print(vp=viewport(x=0.52,y=0.1,width=0.9,height=0.05))
dev.off()

x |> getInfo(obscure=FALSE,nsample=TRUE,nroot=TRUE,ndeme=TRUE) |> unlist()
x |> getInfo(obscure=FALSE,genealogy=TRUE)

try(.External(phylopomp:::P_getInfo,object=x,bob=TRUE))
x |> geneal()

try(raw(0) |> geneal())
try(raw(0) |> newick())
try(raw(10) |> newick())

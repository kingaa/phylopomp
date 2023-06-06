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

try(treeplot())

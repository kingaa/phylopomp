library(pomp)
library(phylopomp)
set.seed(147469846)

png(filename="diagram-01.png",res=100,
  width=1006,height=250,units="px")
freeze(simulate("SIR",time=1),seed=147469846) |> diagram()
dev.off()

png(filename="diagram-02.png",res=100,
  width=618,height=165,units="px")
freeze(simulate("SIIR",time=0.3),seed=788520677) |> diagram()
dev.off()

png(filename="diagram-03.png",res=100,
  width=1053,height=307,units="px")
freeze(simulate("SIIR",time=0.3),seed=459939038) |> diagram(prune=FALSE)
dev.off()

simulate("SIIR",time=0.3) |>
  getInfo(description=TRUE) |>
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

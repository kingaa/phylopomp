library(phylopomp)

set.seed(94429846)
png(filename="diagram-01.png",res=100,
  width=240,height=291,units="px")
simulate("SIR",time=1) |> diagram()
dev.off()
png(filename="diagram-02.png",res=100,
  width=618,height=165,units="px")
simulate("SIIR",time=0.3) |> diagram()
dev.off()

simulate("SIIR",time=0.3) |>
  getInfo(description=TRUE) |>
  getElement("description") |>
  cat()

simulate("SIR",time=1) |>
  getInfo(yaml=TRUE,t0=TRUE) |>
  getElement("yaml") |>
  cat()

try(
  simulate("SIR",time=1) |>
    simulate(time=0.1)
)

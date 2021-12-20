library(phylopomp)

set.seed(94429846)
png(filename="misc-01.png",res=100,
  width=178,height=217,units="px")
simulate("SIR",time=1) |> diagram()
dev.off()
png(filename="misc-02.png",res=100,
  width=1024,height=175,units="px")
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

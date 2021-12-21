library(phylopomp)
set.seed(94329846)

png(filename="diagram-01.png",res=100,
  width=318,height=256,units="px")
simulate("SIR",time=1) |> diagram()
dev.off()

png(filename="diagram-02.png",res=100,
  width=511,height=134,units="px")
simulate("SIIR",Beta2=20,psi2=0.1,I2_0=2,time=0.5) |> diagram()
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

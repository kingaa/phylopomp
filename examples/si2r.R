simulate("SI2R",time=1) |>
  plot(obscure=FALSE)

runSI2R(Beta=10,S0=2000,time=1,psi1=0) |>
  simulate(time=2,psi1=1) |>
  plot(points=TRUE,obscure=FALSE)

library(ggplot2)
simulate("SI2R",time=5) |>
  lineages() |>
  ggplot(aes(x=time,y=lineages))+
  geom_step()

simulate("SI2R",time=2) |>
  diagram(m=30)

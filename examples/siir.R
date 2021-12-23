simulate("SIIR",time=3,psi1=1,psi2=0) |>
  simulate(Beta1=2,gamma=2,time=10,psi1=10,psi2=1) |>
  plot()

runSIIR(Beta1=10,Beta2=8,S0=200,I1_0=10,I2_0=8,R0=0,time=0,t0=-1) |>
  simulate(psi1=10,time=2) |>
  plot(points=TRUE,prune=FALSE)

library(ggplot2)
simulate("SIIR",Beta1=2,Beta2=50,gamma=1,psi1=2,S0=300,I1_0=20,I2_0=2,time=5) |>
  lineages() |>
  ggplot(aes(x=time,y=lineages))+
  geom_step()

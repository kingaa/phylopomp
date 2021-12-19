simulate("SIIR",Beta1=2,Beta2=4,gamma=1,psi=5,S0=200,I1_0=5,I2_0=5,time=3,t0=0) |>
  continue(Beta1=5,Beta2=10,gamma=2,time=10,psi=10) |>
  plot()

runSIIR(Beta1=10,Beta2=8,gamma=1,psi=2,S0=300,I1_0=2,I2_0=20,R0=0,time=0,t0=-1) |>
  continue(psi=10,time=2) |>
  plot(points=TRUE,prune=FALSE)

library(ggplot2)
simulate("SIIR",Beta1=10,Beta2=8,gamma=1,psi=2,S0=300,I1_0=2,I2_0=20,time=5) |>
  lineages() |>
  ggplot(aes(x=time,y=lineages))+
  geom_step()

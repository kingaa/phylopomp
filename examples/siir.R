playSIIR(Beta1=2,Beta2=4,gamma=1,psi=5,S0=200,I1_0=5,I2_0=5,R0=0,times=0:3,t0=0,tree=TRUE) -> x
playSIIR(x,Beta1=5,Beta2=10,gamma=2,times=4:10,psi=10,tree=TRUE) -> x
plot(x)

playSIIR(Beta1=10,Beta2=8,gamma=1,psi=2,S0=1000,I1_0=2,I2_0=20,R0=0,times=0,t0=-1) |>
  playSIIR(psi=10,times=1:5) |>
  getInfo() -> y
plot(y,points=TRUE)

library(ggplot2)
y$lineages |>
  ggplot(aes(x=time,y=lineages))+
  geom_step()

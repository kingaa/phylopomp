playSIIR(Beta1=2,Beta2=4,gamma=1,psi=5,S0=100,I1_0=5,I2_0=1,R0=0,times=0:5,t0=0,tree=TRUE) -> x
playSIIR(x,times=6:10,psi=5,tree=TRUE) -> x
plot(x)

playSIIR(Beta1=3,Beta2=4,gamma=1,psi=2,S0=10,I1_0=5,I2_0=1,R0=0,times=0:5,t0=-1,tree=TRUE) -> x
plot(x,points=TRUE)

y <- getInfo(x)
plot(y,points=TRUE)

library(ggplot2)
y$lineages |>
  ggplot(aes(x=time,y=lineages))+
  geom_step()

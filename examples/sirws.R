playSIRwS(beta=2,gamma=1,psi=2,S0=1000,I0=5,times=0:5,t0=0,tree=TRUE) -> x
playSIRwS(x,times=6:10,psi=1,tree=TRUE) -> x
plot(x)

playSIRwS(beta=3,gamma=1,psi=2,S0=10,I0=5,times=0:5,t0=-1,tree=TRUE) -> x
plot(x,points=TRUE)

y <- getInfo(x)
plot(y,points=TRUE)

library(ggplot2)
y$lineages |>
  ggplot(aes(x=time,y=lineages))+
  geom_step()

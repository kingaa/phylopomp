playmultiSIRwS(beta=10,gamma=1,psi=.5,theta=100.5,S0=500,I0=5,R0=0,times=0:5,t0=-1,tree=TRUE) -> x
plot(x,points=TRUE)

y <- getInfo(x,compact=TRUE)
plot(y,points=TRUE)

library(ggplot2)
y$lineages %>%
  ggplot(aes(x=time,y=lineages))+
  geom_step()

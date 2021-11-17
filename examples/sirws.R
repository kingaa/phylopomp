playSIRwS(beta=2,gamma=1,psi=2,S0=1000,I0=5,R0=0,times=0:5,t0=0,tree=TRUE) -> x
playSIRwS(x,times=6:10,psi=1,tree=TRUE) -> x
plot(x)

playSIRwS(beta=3,gamma=1,psi=2,S0=10,I0=5,R0=0,times=0:5,t0=-1,tree=TRUE) -> x
plot(x,points=TRUE)

y <- getInfo(x,compact=TRUE)
plot(y,points=TRUE)

library(ggplot2)
y$lineages %>%
  ggplot(aes(x=time,y=lineages))+
  geom_step()

playSIRwS(beta=2,gamma=1,psi=2,S0=1000,I0=5,R0=0,times=5,t0=0,tree=TRUE) %>%
  getInfo(compact=TRUE) -> x

plot(x, points=TRUE)

library(pomp)
x$tree %>%
  nwk2df(time=5) %>%
  sir_pomp(beta=2,gamma=1,psi=2,S0=1000,I0=5,R0=0,N=1005,method="gillespie") %>%
  pfilter(Np=2000) %>%
  logLik()


x$tree %>%
  nwk2df(time=5) %>%
  sir_pomp(beta=2,gamma=1,psi=2,S0=1000,I0=5,R0=0,N=1005,method="euler",delta.t=.001) %>%
  pfilter(Np=2000) %>%
  logLik()

playmultiSIRwS(Beta=10,gamma=1,psi=.5,theta=100.5,S0=500,I0=5,R0=0,times=0:5,t0=-1,tree=TRUE) -> x
plot(x,points=TRUE)

y <- getInfo(x,compact=TRUE)
plot(y,points=TRUE)

library(ggplot2)
y$lineages |>
  ggplot(aes(x=time,y=lineages))+
  geom_step()


playmultiSIRwS(Beta=5,gamma=1,psi=.5,theta=100.5,S0=500,I0=5,R0=0,times=5,t0=0,tree=TRUE) |>
  getInfo(compact=TRUE) -> x

plot(x,points=TRUE)

library(pomp)

x$tree |>
  newick2df(time=5) |>
  multisir_pomp(Beta=5,gamma=1,psi=.5,theta=100.5,S0=500,I0=5,R0=0) |>
  pfilter(Np=2000) |>
  logLik()

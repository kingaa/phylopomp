playLBDP(lambda=1,mu=0.5,psi=1,n0=3,times=seq(0,4,by=0.2),tree=TRUE) -> x
plot(x,points=TRUE)

y <- getInfo(x,prune=FALSE)
plot(y,points=TRUE)

library(ggplot2)
y$lineages |>
  ggplot(aes(x=time,y=lineages))+
  geom_step()+
  geom_vline(xintercept=y$etimes,alpha=0.1)

y$cumhaz |>
  ggplot(aes(x=exp(-Lambda)))+
  stat_ecdf()+
  geom_abline(slope=1)

playLBDP(lambda=2,mu=1,psi=0.5,n0=1,times=5) |>
  getInfo() -> x
plot(getInfo(x,compact=TRUE),points=TRUE)

library(pomp)
x$tree |>
  newick2df(time=5) |>
  lbdp_pomp(psi=0.5,lambda=2,mu=1,n0=1) |>
  pfilter(Np=2000) |>
  logLik()

x$tree |>
  newick2df(time=5) |>
  lbdp_exact(psi=0.5,lambda=2,mu=1)





playLBDP(lambda=1,mu=0.5,psi=1,n0=3,times=seq(0,4,by=0.2),tree=TRUE) -> x
plot(x,points=TRUE)

y <- getInfo(x,prune=FALSE)
plot(y,points=TRUE)

library(ggplot2)
y$lineages %>%
  ggplot(aes(x=time,y=lineages))+
  geom_step()+
  geom_vline(xintercept=y$etimes,alpha=0.1)

y$cumhaz %>%
  ggplot(aes(x=exp(-Lambda)))+
  stat_ecdf()+
  geom_abline(slope=1)

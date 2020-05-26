tout <- seq(0,5,by=1)
playSIRwS(beta=2,gamma=1,psi=2,iota=0,S0=1000,I0=5,times=tout,t0=0,tree=TRUE) -> x

playSIRwS(x,times=6:10,psi=0.1,tree=TRUE) -> x
treeplot(x)

y <- getInfo(x)
treeplot(y,points=TRUE)

y[c("etimes","lineages")] %>%
  as_tibble() %>%
  ggplot(aes(x=etimes,y=lineages))+
  geom_step()+
  labs(x="time")

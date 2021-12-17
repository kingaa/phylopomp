library(cowplot)
playSIR(Beta=3,gamma=0.1,psi=0.2,S0=100,I0=5,R0=0,times=2,t0=0) -> x
y <- getInfo(x,prune=FALSE,compact=FALSE)
plot(y,points=TRUE)
y <- getInfo(x)
plot_grid(plotlist=list(plot(y,points=TRUE)[[1]],diagram(y)),
  ncol=1,rel_heights=c(2,1))

plot_grid(diagram(y))

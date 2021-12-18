library(cowplot)
runSIR(Beta=3,gamma=0.1,psi=0.2,S0=100,I0=5,R0=0,time=2,t0=0) -> x
plot(x,points=TRUE,prune=FALSE,compact=FALSE)
plot_grid(plotlist=list(plot(x,points=TRUE)[[1]],diagram(x)),
  ncol=1,rel_heights=c(4,1))

library(tidyverse)
playMoran(n=5,mu=10,times=c(0,seq(100,200,by=25)),
  stationary=FALSE,ill=TRUE,tree=TRUE) -> x
plot(x[5,],points=TRUE,diagram=TRUE)

playMoran(n=8,mu=8,times=100:130,sample=FALSE,tree=TRUE,ill=TRUE) %>%
  mutate(dg=diagram(illustration)) -> x
plot(x,points=TRUE,diagram=TRUE)

playMoran(n=8,mu=8,times=0:30,sample=FALSE,tree=TRUE,ill=TRUE,
  stationary=FALSE) %>%
  mutate(dg=diagram(illustration)) -> x
plot(x,points=TRUE,diagram=TRUE)

library(cowplot)
playMoran(n=5,mu=5,times=0:3,stationary=TRUE,tree=TRUE,ill=TRUE) %>%
  mutate(grob=diagram(illustration)) -> x
plot_grid(plotlist=c(x$grob,treeplot(x,points=TRUE)),ncol=2,byrow=FALSE)

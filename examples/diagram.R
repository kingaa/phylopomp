library(tidyverse)
playMoran(n=5,mu=10,times=c(0,seq(100,200,by=25)),
  stationary=FALSE,ill=TRUE,tree=TRUE) %>%
  mutate(grob=diagram(illustration,fontsize=9)) -> x
plot(x[5,],points=TRUE)[[1]]+
  annotation_custom(x$grob[[5]],xmin=0,xmax=150,ymin=12,ymax=16)

playMoran(n=8,mu=8,times=100:130,sample=FALSE,tree=TRUE,ill=TRUE) %>%
  mutate(dg=diagram(illustration)) -> x
plot(x,points=TRUE) -> pl
lapply(
  seq_along(pl),
  function(k) {
    pl[[k]]+
      expand_limits(y=-5)+
      annotation_custom(
        x$dg[[k]],
        xmin=105,xmax=125,
        ymin=-5,ymax=0
      )
  }
)

playMoran(n=8,mu=8,times=0:30,sample=FALSE,tree=TRUE,ill=TRUE,
  stationary=FALSE) %>%
  mutate(dg=diagram(illustration)) -> x
plot(x,points=TRUE) -> pl
lapply(
  seq_along(pl),
  function(k) {
    pl[[k]]+
      expand_limits(y=-5)+
      annotation_custom(
        x$dg[[k]],
        xmin=5,xmax=25,
        ymin=-5,ymax=0
      )
  }
)

library(cowplot)
playMoran(n=5,mu=5,times=0:3,stationary=TRUE,tree=TRUE,ill=TRUE) %>%
  mutate(grob=diagram(illustration)) -> x
plot_grid(plotlist=c(x$grob,treeplot(x,points=TRUE)),ncol=2,byrow=FALSE)

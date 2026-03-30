library(ggplot2)

runSEIR(t0=-1,time=2,E0=0,I0=3) -> x
runSEIR(t0=0,time=3,E0=2,I0=0) -> y
runSEIR(t0=1,time=4,E0=3,I0=2) -> z

plot_grid(
  plot_grid(
    plotlist=lapply(
      list(x,y,z),
      plot,
      points=TRUE,ladderize=FALSE,
      legend.position="none"
    ) |>
      lapply(\(.).+geom_vline(xintercept=c(-1,0,1,2,3,4))),
    ncol=1,align="v",axis="bltr"
  ),
  plot_grid(
    plotlist=lapply(
      list(x+y,y+z,x+y+z),
      plot,
      points=TRUE,ladderize=FALSE,
      legend.position="none"
    ) |>
      lapply(\(.).+geom_vline(xintercept=c(-1,0,1,2,3,4))),
    ncol=1,align="h",axis="bltr"
  )+
    geom_vline(xintercept=c(0,1,2,3,4)),
  nrow=1
)

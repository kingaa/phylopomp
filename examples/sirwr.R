set.seed(28494712L)
simulate("SIRwr",
         Beta=2,gamma=1,psi=2,
         rhoS=.5, rhoM=.1, rhoL=.3,
         rhoSM=0, rhoSL=.2, rhoML = 0,
         S0=100,I0=10,R0=0,time=3,t0=0) |>
  plot()

set.seed(28494712L)
runSIRwr(Beta=3,gamma=1,psi=2,
         rhoS=0,rhoM=.5,rhoL=0,
         rhoSM=0, rhoSL=0, rhoML = 0,
         S0=20,I0=5,R0=0,time=1,t0=0) |>
  plot(points=TRUE, retimes=TRUE)


# plot_grid(plotlist=list(plot(x,points=TRUE),diagram(x)),
#   ncol=1,rel_heights=c(4,1))
#
# simulate("SIRwr",delta=1,time=20,I0=4) |> plot()
# simulate("SIRwr",delta=1,time=20,I0=4) |> lineages() |> plot()

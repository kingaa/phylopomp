simulate("SIRwr",Beta=2,gamma=1,psi=2,
         rhoS=.5, rhoM=.1, rhoL=.3, 
         rhoSM=0, rhoSL=.2, rhoML = 0,
         S0=100,I0=10,time=5) |>
  plot(prune=TRUE, compact=TRUE, points=TRUE)

runSIRwr(Beta=3,gamma=1,psi=2,
         rhoS=0,rhoM=0.5,rhoL=0,
         rhoSM=0, rhoSL=.2, rhoML = 0,
         S0=20,I0=5,R0=0,time=5,t0=-1) |>
  plot(points=TRUE)
# 
# runSIRwr(Beta=3,gamma=0.1,psi=0.2,S0=100,I0=5,R0=0,time=2,t0=0) -> x
# plot_grid(plotlist=list(plot(x,points=TRUE),diagram(x)),
#   ncol=1,rel_heights=c(4,1))
# 
# simulate("SIRwr",delta=1,time=20,I0=4) |> plot()
# simulate("SIRwr",delta=1,time=20,I0=4) |> lineages() |> plot()

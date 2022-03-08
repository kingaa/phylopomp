simulate("SEIR",Beta=2,sigma=3,gamma=1,psi=2,S0=1000,E0=10,I0=5,time=5) |>
  simulate(Beta=5,sigma=2,gamma=2,time=10,psi=3) |>
  plot()

runSEIR(Beta=3,sigma=2,gamma=1,psi=2,S0=20,I0=5,R0=0,time=5,t0=-1) |>
  plot(points=TRUE)

runSEIR(Beta=3,sigma=2,gamma=0.1,psi=0.2,S0=100,I0=5,R0=0,time=2,t0=0) -> x
plot_grid(plotlist=list(plot(x,points=TRUE),diagram(x)),
  ncol=1,rel_heights=c(4,1))

simulate("SEIR",delta=1,time=20,I0=4) |> plot()
simulate("SEIR",delta=1,time=20,I0=4) |> lineages() |> plot()

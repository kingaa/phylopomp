simulate("LBDP",time=4) |> plot(points=TRUE)

simulate("LBDP",lambda=2,mu=1,psi=3,n0=1,time=1) |>
  simulate(time=10,lambda=1) |>
  plot()

simulate("LBDP",time=4) |>
  lineages() |>
  plot()

## Restrict to trees with 50--500 tips (rejection sampling until in range)
runLBDP(time=4,lambda=1.3,mu=1,psi=0.5,n0=50,min_tips=50,max_tips=500) |>
  getInfo(nsample=TRUE)


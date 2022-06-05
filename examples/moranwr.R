simulate("Moranwr",n=10,time=10,rhoA=1) |> plot(points=TRUE)

simulate("Moranwr",time=100,psi=0,rhoA=1) |>
  simulate(time=101,psi=100) |>
  plot()

simulate("Moranwr",time=100,psi=0,rhoA=1,frac=.5) |>
  batch() |>
  plot()
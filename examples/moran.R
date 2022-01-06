simulate("Moran",time=100) |> plot()

simulate("Moran",time=100,psi=0) |>
  simulate(time=101,psi=100) |>
  plot()

simulate("Moran",time=100,psi=0,n=20) |>
  simulate(time=100.1,psi=100) |>
  diagram(m=20)

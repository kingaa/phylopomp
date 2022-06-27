simulate("LBDPwr",time=4) |> plot(points=TRUE)

set.seed(298759241L)
simulate("LBDPwr",lambda=2,mu=1,psi=3,rhoA=.5,rhoB=0,n0=1,time=1) |>
  simulate(time=10,lambda=1) |>
  plot(points=TRUE)

simulate("LBDPwr",rhoA=.5,time=4) |>
  lineages() |>
  plot()

# a pure birth process
set.seed(12592834L)
simulate("LBDPwr",lambda=1,mu=0,psi=3,rhoA=1,rhoB=0,n0=1,time=4) |>
  plot(points=TRUE)

# a pure death process
## the `coalesent event` is exactly the reassortment
set.seed(10347293L)
simulate("LBDPwr",lambda=0,mu=1,psi=3,rhoA=1,rhoB=0,n0=1e2,time=4) |>
  plot(points=TRUE)

set.seed(28475019L)
simulate("LBDPwr",lambda=2,mu=1,psi=0,rhoA=1,rhoB=0,frac=.2,n0=10,time=2) |>
  batch() |>
  plot(points=TRUE)

simulate("LBDPwr",time=2,cont=FALSE) |> plot(points=TRUE)

set.seed(298759241L)
simulate("LBDPwr",lambda=2,mu=1,psi=.5,rhoA=.5,rhoB=0,n0=1,time=1,cont=FALSE) |>
  simulate(time=10,lambda=1) |>
  plot(points=TRUE)

simulate("LBDPwr",time=4,cont=FALSE) |>
  lineages() |>
  plot()

# a pure birth process
set.seed(12592834L)
simulate("LBDPwr",lambda=1,mu=0,psi=3,rhoA=1,rhoB=0,n0=1,time=4,cont=FALSE) |>
  plot(points=TRUE)

# a pure death process
## the `coalesent event` is exactly the reassortment
set.seed(10347293L)
simulate("LBDPwr",lambda=0,mu=1,psi=3,rhoA=1,rhoB=0,n0=1e2,time=4,cont=FALSE) |>
  plot(points=TRUE)

set.seed(28475019L)
simulate("LBDPwr",lambda=2,mu=1,psi=0,rhoA=1,rhoB=0,frac=.2,n0=10,time=2) |>
  batch() |>
  plot(points=TRUE)


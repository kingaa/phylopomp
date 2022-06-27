simulate("SIRwr",Beta=2,gamma=1,psi=2,frac=0,
         rhoS=.5, rhoM=.1, rhoL=.3,
         rhoSM=0, rhoSL=.2, rhoML = 0,
         S0=100,I0=10,time=5) |>
  plot(prune=TRUE, compact=TRUE, points=TRUE)

runSIRwr(Beta=3,gamma=1,psi=2,frac=0,
         rhoS=0,rhoM=.5,rhoL=0,
         rhoSM=0, rhoSL=0, rhoML = 0,
         S0=20,I0=5,R0=0,time=5,t0=-1) |>
  plot(prune=TRUE, compact=TRUE, points=TRUE, t0=-1)

runSIRwr(Beta=3,gamma=1,psi=0,frac=.8,
         rhoS=0,rhoM=.5,rhoL=0,
         rhoSM=0, rhoSL=0, rhoML = 0,
         S0=100,I0=10,R0=0,time=1,t0=0) |>
  batch() |>
  plot()

runSIR(Beta=2,gamma=1,psi=2,S0=100,I0=2,R0=0,time=10,t0=0) |>
  getInfo(tree=TRUE) |>
  getElement("tree") |>
  newick2df(time=5)

playSIR(Beta=2,gamma=1,psi=2,S0=100,I0=2,R0=0,times=c(0,5),t0=0,tree=TRUE) -> x
y <- getInfo(x)
newick2df(y$tree) -> z

# compare to y$lineages
y$lineages |>
  all.equal(tail(z,-1)[,1:2],tolerance=1e-5)

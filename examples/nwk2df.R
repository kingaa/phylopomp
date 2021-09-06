playSIRwS(beta=2,gamma=1,psi=2,S0=100,I0=2,R0=0,times=c(0,5),t0=0,tree=TRUE) -> x
y <- getInfo(x)
nwk2df(y$tree, time=5, show_branches=TRUE) -> z

# compare to y$lineages
y$lineages %>%
  all.equal(tail(z,-1)[,1:2],tolerance=1e-5)

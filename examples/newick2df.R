playSIR(Beta=2,gamma=1,psi=2,S0=100,I0=2,R0=0,times=c(0,5),t0=0) -> x
y <- getInfo(x)
newick2df(y$tree) -> z

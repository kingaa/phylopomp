playSIRwS(beta=2,gamma=1,psi=2,S0=100,I0=2,times=5,t0=0,tree=TRUE) -> x
y <- getInfo(x)
newick2df(y$tree) -> z
head(z)

# compare to y
head(y$lineages)

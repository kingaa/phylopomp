runSIR(Beta=2,gamma=1,psi=2,S0=100,I0=2,R0=0,time=10,t0=0) -> x

x |> lineages()

x |> newick() |> newick2df()

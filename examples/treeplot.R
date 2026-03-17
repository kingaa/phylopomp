## Simulate and plot a segmented genealogy
x <- runLBDPwr(time=5, lambda=2, mu=1, psi=1,
               rhoA=0.5, rhoB=0.5, n0=5)

## Plot all segments (faceted)
plot(x)

## Plot with pruning, obscuring, and hiding reassortment markers
plot(x, prune=TRUE, obscure=TRUE, hide=TRUE)

## Use treeplot directly with a single Newick string (first segment)
nw <- newick(x)
treeplot(nw[1], t0=0, time=5)

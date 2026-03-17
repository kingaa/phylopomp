## Simulate a linear birth-death process with reassortment
x <- runLBDPwr(time=5, lambda=2, mu=1, psi=1,
               rhoA=0.5, rhoB=0.5, n0=5)
x

## Extract per-segment Newick trees
newick(x)

## Extract genealogy data frames
gendat(x)

## Extract lineage counts
lineages(x)

## Continue simulation
y <- continueLBDPwr(x, time=10)
y

## Extract bare per-segment genealogies
geneal(x)

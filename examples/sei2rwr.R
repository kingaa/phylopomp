## Simulate a two-class SEIRS model with reassortment
y <- runSEI2Rwr(time=5, Beta0=4, sigma=1, gamma=1, psi=1, omega=0.1,
                rhoA=0.1, rhoB=0.05,
                S0=100, E0=5, I0=5, R0=0)
y

## plot the trees
plot(y,points=TRUE,obscure=FALSE,hide=FALSE)

## Get full info
getInfo(y, tree=TRUE, t0=TRUE, time=TRUE, gendat=TRUE)

## Extract per-segment Newick trees
newick(y)

## Extract genealogy data frames
gendat(y)

## Continue simulation with changed parameters
z <- continueSEI2Rwr(y, time=10, rhoA=0.2)
z
plot(z,points=TRUE,obscure=FALSE,hide=FALSE)

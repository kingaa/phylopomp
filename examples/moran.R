playMoran(n=5,mu=5,times=1:10,t0=0,tree=TRUE) -> x
playMoran(x,times=11:20,tree=TRUE) -> x
plot(x)

playMoran(n=20,mu=20,times=1:20,stationary=FALSE,tree=TRUE) -> x
plot(x)

y <- getInfo(x,prune=FALSE)
plot(y,points=TRUE)

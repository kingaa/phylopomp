playMoran(n=5,mu=5,times=0:10,t0=0,tree=TRUE,ill=TRUE) -> x
playMoran(x,times=11:20,tree=TRUE) -> x
plot(x)

playMoran(n=20,mu=20,times=0:20,stationary=FALSE,tree=TRUE,ill=TRUE) -> x
plot(x,points=TRUE)

y <- getInfo(x,prune=FALSE)
plot(y,points=TRUE)

playMoran(n=5,mu=5,t0=-1,times=0:3,stationary=FALSE,tree=TRUE,ill=TRUE) -> x
plot(x,points=TRUE)

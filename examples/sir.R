playSIR(Beta=2,gamma=1,psi=2,S0=1000,I0=5,R0=0,times=0:5,t0=0,tree=TRUE) -> x
playSIR(x,Beta=5,gamma=2,times=6:10,psi=3,tree=TRUE) -> x
plot(x)

playSIR(Beta=3,gamma=1,psi=2,S0=10,I0=5,R0=0,times=0:5,t0=-1,tree=TRUE) -> x
plot(x,points=TRUE)

y <- getInfo(x)
plot(y,points=TRUE)

playSIR(Beta=30,gamma=1,psi=2,S0=100,I0=5,R0=0,times=0.1,t0=0) -> x
y <- getInfo(x,prune=FALSE,compact=FALSE)
plot(y,points=TRUE)

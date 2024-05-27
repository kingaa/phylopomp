png(filename="gendat-%0d.png",res=100,
  width=1114,height=233,units="px")

library(phylopomp)
set.seed(1888056571)

runSIRS(time=4,psi=0.3) -> x

plot(x,points=TRUE)

diagram(x)

x |>
  gendat() |>
  print(n=100)

dev.off()

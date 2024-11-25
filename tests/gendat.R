png(filename="gendat-%0d.png",res=100,
  width=1114,height=233,units="px")

library(phylopomp)
set.seed(1888056571)

runSIRS(time=4,psi=0.3) -> x

plot(x,points=TRUE)

diagram(x)

x |>
  gendat() -> g

g

stopifnot(identical(g,getInfo(x,gendat=TRUE)$gendat))

g |>
  with({
    stopifnot(
      diff(c(index,max(index)))==saturation
    )
    for (i in seq_len(length(ancestor))) {
      if (saturation[i] > 0) {
        k <- seq.int(from=index[i],to=index[i]+saturation[i]-1,by=1)
        stopifnot(i==ancestor[child[k+1]+1]+1)
      }
    }
  })

freeze(
  seed=540737457,
  runSEIRS(time=40,omega=3,psi=0.3)
) |>
  gendat() -> g

g |>
  with({
    stopifnot(
      diff(c(index,max(index)))==saturation
    )
    for (i in seq_len(length(ancestor))) {
      if (saturation[i] > 0) {
        k <- seq.int(from=index[i],to=index[i]+saturation[i]-1,by=1)
        stopifnot(i==ancestor[child[k+1]+1]+1)
      }
    }
  })

dev.off()

simulate("SI2R",time=1) |>
  plot(obscure=FALSE)

runSI2R(Beta=10,S0=2000,time=1,psi1=0) |>
  simulate(time=2,psi1=1) |>
  plot(points=TRUE,obscure=FALSE)

library(ggplot2)
simulate("SI2R",time=5) |>
  lineages() |>
  ggplot(aes(x=time,y=lineages))+
  geom_step()

simulate("SI2R",time=2) |>
  diagram(m=30)

simulate("SI2R",time=20,delta=0.2,mu=20) -> x
plot_grid(
  x |> plot(obscure=FALSE),
  x |> lineages(obscure=FALSE) |>
    gather(var,val,-time) |>
    ggplot(aes(x=time,y=val,color=var,group=var))+
    geom_step()+
    labs(y="lineages")+
    guides(color="none")+
    theme_classic(),
  ncol=1,
  align="v",axis="b"
)

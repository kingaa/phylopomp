pal <- c("#000000","#00274c","#ffcb05")

simulate("SIIR",time=3,psi1=1,psi2=1) -> x
plot_grid(
  x |> plot(palette=pal),
  x |> lineages() |> plot(palette=pal),
  x |> plot(obscure=FALSE,palette=pal),
  x |> lineages(obscure=FALSE) |>
    plot(palette=pal,legend.position=c(0.8,0.9)),
  align="v",axis="b",
  ncol=2,byrow=FALSE
)

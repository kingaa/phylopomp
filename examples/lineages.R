library(tidyverse)

pal <- c("#00274c","#ffcb05")

simulate("SIIR",time=3) -> x
plot_grid(
  x |> plot(),
  x |> lineages() |> plot.gplin(),
  x |> plot(obscure=FALSE,palette=pal),
  x |> lineages(obscure=FALSE) |>
    getElement(1) |>
    plot(palette=pal,legend.position=c(0.8,0.9)),
  align="v",axis="b",
  ncol=2,byrow=FALSE
)

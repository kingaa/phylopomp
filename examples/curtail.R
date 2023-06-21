library(ggplot2)

simulate("SIIR",time=5) -> x

plot_grid(
  x |>
    plot(prune=FALSE,points=TRUE),
  x |>
    curtail(time=3) |>
    plot(prune=FALSE,points=TRUE)+
    expand_limits(x=5),
  ncol=1,align="h",axis="tblr"
)

plot_grid(
  x |>
    plot(prune=TRUE,points=TRUE)+
    geom_vline(xintercept=3),
  x |> curtail(time=3) |>
    plot(prune=TRUE,points=TRUE)+
    geom_vline(xintercept=3)+
    expand_limits(x=5),
  ncol=1,align="h",axis="tblr"
)

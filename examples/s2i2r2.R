runS2I2R2(
  time=30,
  iota2=0.01,Beta12=0.1,
  I1_0=0,I2_0=0,
  psi1=10,psi2=10,
  omega1=0.2,
  b2=0.02,d2=0.02
) |>
  freeze(seed=847110120) -> x

plot_grid(
  x |>
    plot(
      obscure=FALSE,
      palette=c("#000000ff","#440154ff","#21908cff","#ccccccff"),
      legend.position="inside",
      legend.position.inside=c(0.7,0.7)
    ),
  x |>
    lineages(obscure=FALSE) |>
    plot(
      palette=c("#000000ff","#440154ff","#21908cff","#ccccccff"),
      legend.position="none"
    ),
  ncol=1,
  align="hv",
  axis="b"
)

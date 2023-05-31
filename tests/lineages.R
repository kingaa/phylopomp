png(filename="lineages-%02d.png",res=100,width=6,height=4,units="in")

suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(481604604)
options(digits=3)

simulate("SI2R",time=1) -> x

bind_rows(
  A=x |> lineages(),
  B=x |> lineages(obscure=FALSE) |>
    group_by(time) |>
    summarize(
      lineages=sum(lineages),
      saturation=sum(saturation)
    ) |>
    ungroup(),
    .id="method"
) |>
  ggplot(aes(x=time,y=lineages,color=method,group=method))+
  geom_step()

pal <- c("#00274c","#ffcb05","#006597")

plot_grid(
  plot_grid(
    x |> plot(),
    x |> plot(obscure=FALSE,palette=pal),
    nrow=1,align="h",axis="l"
  ),
  plot_grid(
    x |> lineages() |> plot(),
    x |> lineages(obscure=FALSE) |>
      select(time,deme,lineages) |>
      pivot_wider(names_from=deme,values_from=lineages,names_prefix="deme") |>
      mutate(total=deme1+deme2) |>
      pivot_longer(-time) |>
      ggplot(aes(x=time,y=value,color=name))+
      geom_step()+
      scale_color_manual(values=pal)+
      guides(color="none")+
      theme_classic(),
    nrow=1,align="hv",axis="tblr"
  ),
  nrow=2
)

simulate("SIIR",time=5,S0=50,psi2=1,sigma12=1,I1_0=3,I2_0=3) -> x

plot_grid(
  plot_grid(
    x |> plot(prune=F,obscure=F,points=T,palette=pal),
    x |> lineages(prune=F,obscure=F) |>
      select(time,deme,lineages) |>
      pivot_wider(names_from=deme,values_from=lineages,names_prefix="deme") |>
      mutate(total=deme1+deme2) |>
      pivot_longer(-time) |>
      ggplot(aes(x=time,y=value,color=name,group=name))+
      scale_color_discrete(type=pal)+
      geom_step()+
      theme_classic()+
      guides(color="none"),
    ncol=1,align='v',axis='b'
  ),
  x |> diagram(prune=F,obscure=F),
  ncol=1,
  rel_heights=c(2,0.3)
)

try(x |> lineages(obscure=FALSE) |> plot(palette=pal[1]))

dev.off()

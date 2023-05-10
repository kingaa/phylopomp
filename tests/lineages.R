png(filename="lineages-%02d.png",res=100,width=6,height=4,units="in")

suppressPackageStartupMessages({
  library(phylopomp)
  library(tidyverse)
})
theme_set(theme_bw())
set.seed(481604604)
options(digits=3)

simulate("SI2R",time=1) -> x

bind_rows(
  A=x |> lineages(),
  B=x |> lineages(obscure=FALSE) |>
    mutate(lineages=deme1+deme2) |>
    select(time,lineages),
  .id="method"
) |>
  ggplot(aes(x=time,y=lineages,color=method,group=method))+
  geom_step()

plot_grid(
  plot(x),
  x |> lineages() |> plot(),
  x |> plot(obscure=FALSE,palette=c("#00274c","#ffcb05")),
  x |>
    lineages(obscure=FALSE) |>
    mutate(total=deme1+deme2) |>
    gather(var,val,-time) |>
    ggplot(aes(x=time,y=val,color=var,group=var))+
    geom_step()+
    guides(color="none")+
    scale_color_manual(values=c("#00274c","#ffcb05","#006597"))+
    theme_classic(),
  align="v",axis="b",
  ncol=2,byrow=FALSE
)

pal <- c("#00274c","#ffcb05","#006597")

simulate("SIIR",time=5,S0=50,psi2=1,sigma12=1,I1_0=3,I2_0=3) -> x

plot_grid(
  plot_grid(
    x |> plot(prune=F,obscure=F,points=T,palette=pal),
    x |> lineages(prune=F,obscure=F) |>
      mutate(lineages=deme1+deme2) |>
      gather(var,val,-time) |>
      ggplot(aes(x=time,y=val,color=var,group=var))+
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

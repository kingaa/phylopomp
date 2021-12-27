library(tidyverse)

simulate("SIIR",time=3) -> x

plot_grid(
  plot(x),
  x |>
    lineages() |>
    ggplot(aes(x=time,y=lineages))+
    geom_step()+
    theme_classic(),
  plot(x,obscure=FALSE,palette=c("#00274c","#ffcb05")),
  x |>
    lineages(obscure=FALSE) |>
    gather(var,val,deme1,deme2) |>
    ggplot(aes(x=time,y=val,color=var,group=var))+
    geom_step()+
    scale_color_manual(values=c("#00274c","#ffcb05"))+
    theme_classic()+
    labs(color="")+
    theme(legend.position=c(0.8,0.9)),
  align="v",axis="b",
  ncol=2,byrow=FALSE
)

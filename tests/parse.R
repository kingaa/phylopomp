png(filename="parse-%02d.png",res=100,width=6,height=4,units="in")

suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(4963811)
options(digits=3)

runSEIR(time=3,I0=3) |>
  newick(prune=FALSE,obscure=FALSE) -> tree
plot_grid(
  tree |> treeplot(points=TRUE),
  tree |> parse_newick() |>
    getInfo(prune=FALSE,obscure=FALSE,newick=TRUE) |>
    getElement("newick") |>
    treeplot(points=TRUE),
  nrow=1
)

runSEIR(time=3,I0=3) |>
  newick(prune=FALSE,obscure=TRUE) -> tree
plot_grid(
  tree |> treeplot(points=TRUE),
  tree |> parse_newick() |>
    getInfo(prune=FALSE,obscure=TRUE,newick=TRUE) |>
    getElement("newick") |>
    treeplot(points=TRUE),
  nrow=1
)

runSEIR(time=30,I0=3) |>
  newick(prune=FALSE,obscure=TRUE) -> tree
plot_grid(
  tree |> treeplot(points=TRUE),
  tree |> parse_newick() |>
    getInfo(prune=FALSE,obscure=TRUE,newick=TRUE) |>
    getElement("newick") |>
    treeplot(points=TRUE),
  nrow=1
)

runSEIR(time=3,I0=3) |>
  newick(prune=TRUE,obscure=FALSE) -> tree
plot_grid(
  tree |> treeplot(points=TRUE),
  tree |> parse_newick() |>
    getInfo(prune=TRUE,obscure=FALSE,newick=TRUE) |>
    getElement("newick") |>
    treeplot(points=TRUE),
  nrow=1
)

runSEIR(time=5,I0=3) -> x
x |>
  newick(prune=TRUE,obscure=TRUE) -> tree
plot_grid(
  x |> plot(points=TRUE,prune=TRUE,obscure=TRUE),
  tree |> parse_newick() |>
    getInfo(prune=TRUE,obscure=TRUE,newick=TRUE) |>
    getElement("newick") |>
    treeplot(points=TRUE),
  nrow=1
)

runSEIR(time=5,I0=3) -> x

plot_grid(
  x |> plot(prune=TRUE,obscure=FALSE,points=TRUE),
  x |> lineages(prune=TRUE,obscure=FALSE) |> plot(legend.position="none"),
  x |> newick(prune=TRUE,obscure=FALSE) |>
    parse_newick() |>
    getInfo(prune=TRUE,obscure=FALSE,lineages=TRUE,description=TRUE,
      time=TRUE,yaml=TRUE,structure=TRUE) |>
    getElement("lineages") |>  
    plot(legend.position="none")+
    expand_limits(x=5),
  align="hv",axis="tblr",
  ncol=1,rel_heights=c(2,1,1)
)

stopifnot(
  all.equal(
    x |> lineages(prune=FALSE,obscure=FALSE) |>
      slice(1:20),
    x |> newick(prune=FALSE,obscure=FALSE) |>
      parse_newick() |>
      getInfo(prune=FALSE,obscure=FALSE,lineages=TRUE) |>
      getElement("lineages") |>
      slice(1:20),
    tolerance=1e-4
  )
)

try(
  r"{(((p_9_0:1.0000,b_3_5:0.999,o_1_8:0.5555)g_0_88:0.5;}" |>
     parse_newick()
)

try(
  r"{((o_9_1:1.000000,b_3_2:0.500000,o_1_3:1.000000)m_0_0:0.000000);}" |>
     parse_newick()
)

try(
  r"{(o_9_1:1.000000,b_3_2:0.500000,o_1_3:1.000000)m_0_0:0.000000);}" |>
     parse_newick()
)

try(
  r"{(o_9_1:1.000000,o_3_2:0.500000,o_1_3:1.000000)m_0_0:0.000000)()));}" |>
     parse_newick()
)

r"{(o_9_1:1.000000,b_1_3:1.000000)m_0_0:0.000000}" |>
   parse_newick() |>
   getInfo(prune=FALSE,obscure=FALSE,lineages=FALSE,newick=TRUE)

try(
  r"{(o_9_1:1.000000,m_1_3:1.000000)m_0_0:0.000000;}" |>
  parse_newick()
)

try(
  r"{(o_9_1:1.000000)_g_1_3:1.000000)m_0_0:0.000000;}" |>
     parse_newick()
)

r"{((,o_9_1:1.000000,)g_1_3:1.000000)m_0_0:0.000000;}" |>
   parse_newick() |>
   getInfo(prune=FALSE,obscure=FALSE,lineages=FALSE,newick=TRUE)

r"{(o_9_1:1.000000,b_0_0:3,,)m_0_0:0.000000;}" |>
   parse_newick() |>
   getInfo(prune=FALSE,obscure=FALSE,lineages=FALSE,newick=TRUE)

try(
  r"{(,o_9_1:1.000000,)g_1_3:1.000000,h)m_0_0:0.000000))));}" |>
     parse_newick()
)

try(
  r"{(,o_9_1:1.000000,)g_1_3:1.000000,)m_0_0:0.000000))y));}" |>
     parse_newick()
)

try(
  r"{((((o_9_1:1.000000)g_1_3:1.000000)m_0_0:0.000000)g_3:22)()))));}" |>
     parse_newick()
)

try(
  r"{(o_9_1:1.000000,b_1_3)m_0_0:0.000000;}" |>
     parse_newick()
)

r"{(o_9_1:1.000000,b_0_0:3;;;)m_0_0:0.000000;}" |>
   parse_newick() |>
   getInfo(prune=FALSE,obscure=FALSE,lineages=FALSE,newick=TRUE)

r"{(o_9_:1.000000;;;b_0:3;;)m_0:0.000000;}" |>
   parse_newick() |>
   getInfo(prune=FALSE,obscure=FALSE,lineages=FALSE,newick=TRUE)

try(
  r"{(o_9_1:1.000000,b_0_0:3)m_0_0:0.000000,b_4_42:abc}" |>
     parse_newick()
)

r"{(o_9_1:1.000000,b_0:3,)m_0_0:0.000000,b_2_45:17}" |>
   parse_newick() |>
   getInfo(prune=FALSE,obscure=FALSE,lineages=FALSE,newick=TRUE)

r"{(o_9_1:1.000000,b_0_0:3)m_0_0:0.000000,b_____2:17;}" |>
   parse_newick() |>
   getInfo(prune=FALSE,obscure=FALSE,lineages=FALSE,newick=TRUE)

try(
  r"{(o_a9_1:1.000000,b_0_0:3)m_0_0:0.000000,b_____2:17;}" |>
     parse_newick()
)

try(
  r"{(o_9_1:1.000000,b_0_0:3)m_0_0(:0.500000,b_____2:17;}" |>
     parse_newick()
)

try(
  r"{(o_9_1:1.000000,b_0_0:3)m_)0_0:0.500000,b_____2:17;}" |>
     parse_newick()
)

try(
  r"{(o_9_1:1.000000,b_0_0:3)m_0_0:(0.000000,b_____2:17;}" |>
     parse_newick()
)

try(
  r"{(o_9_1:1.000000,b_0_0:3)m_0_0:0.000000,(b_____2:17;}" |>
     parse_newick()
)

r"{(o_9_1:  1.000000,b_0:3,)m_0_0:0.000000,b_2_45:17}" |>
   parse_newick() |>
   getInfo(prune=FALSE,obscure=FALSE,lineages=FALSE,newick=TRUE)

r"{(o_9_1:  1.000000,b_0:3,  )m_0_0:0.000000,b_2_45:17}" |>
   parse_newick() |>
   getInfo(prune=FALSE,obscure=FALSE,lineages=FALSE,newick=TRUE)

r"{(o_9_1:  1.000000,b_0:3,  )m_0_0:0.000000,  b_2_45:17}" |>
   parse_newick() |>
   getInfo(prune=FALSE,obscure=FALSE,lineages=FALSE,newick=TRUE)

r"{(o_9_1:  1.000000,b_0:3,  )m_0_0:0.000000,
       b_2____45:17}" |>
   parse_newick() |>
   getInfo(prune=FALSE,obscure=FALSE,lineages=FALSE,newick=TRUE)

try(
  r"{(o_9_1:1.000000,b_0_0:3))m_0_0:0.000000,b_____2:17;}" |>
     parse_newick()
)

try(
  r"{(o_9_1:1.000000,b_0_0:3)m_0_0:0.000000,b_____2:17))));}" |>
     parse_newick()
)

try(r"{((o_9:20,b_9:20)g_9:10,b_9:10)m______}" |> parse_newick())

dev.off()

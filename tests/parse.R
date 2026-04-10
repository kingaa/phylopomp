png(filename="parse-%02d.png",res=100,width=6,height=4,units="in")

options(tidyverse.quiet=TRUE,digits=3)
suppressPackageStartupMessages({
  library(tidyverse)
  library(phylopomp)
})
theme_set(theme_bw())
set.seed(4963811)

runSEIR(time=3,S0=100,I0=3,E0=5,pop=108) |>
  newick(prune=TRUE,obscure=FALSE) -> tree
plot_grid(
  tree |> phylopomp:::treeplot(points=TRUE,t0=0,time=3),
  tree |> parse_newick() |>
    getInfo(obscure=FALSE,newick=TRUE) |>
    getElement("newick") |>
    phylopomp:::treeplot(points=TRUE,t0=0,time=3),
  nrow=1
)

runSEIR(time=3,S0=100,I0=3,E0=5,pop=108) |>
  newick(prune=TRUE,obscure=TRUE) -> tree
plot_grid(
  tree |> phylopomp:::treeplot(points=TRUE,t0=0,time=3),
  tree |> parse_newick() |>
    getInfo(prune=FALSE,obscure=TRUE,newick=TRUE) |>
    getElement("newick") |>
    phylopomp:::treeplot(points=TRUE,t0=0,time=3),
  nrow=1
)

runSEIR(time=10,S0=100,I0=3,E0=5,pop=108) |>
  newick(prune=TRUE,obscure=TRUE) -> tree
plot_grid(
  tree |> phylopomp:::treeplot(points=TRUE,t0=0,time=10),
  tree |> parse_newick() |>
    getInfo(prune=FALSE,obscure=TRUE,newick=TRUE) |>
    getElement("newick") |>
    phylopomp:::treeplot(points=TRUE,t0=0,time=10),
  nrow=1
)

runSEIR(time=3,S0=100,I0=3,E0=5,pop=108) |>
  newick(prune=TRUE,obscure=FALSE) -> tree
plot_grid(
  tree |> phylopomp:::treeplot(points=TRUE,t0=0,time=3),
  tree |> parse_newick() |>
    getInfo(prune=TRUE,obscure=FALSE,newick=TRUE) |>
    getElement("newick") |>
    phylopomp:::treeplot(points=TRUE,t0=0,time=3),
  nrow=1
)

runSEIR(time=5,S0=100,I0=3,E0=5,pop=108) -> x
x |>
  newick(prune=TRUE,obscure=TRUE) -> tree
plot_grid(
  tree |> phylopomp:::treeplot(points=TRUE,t0=0,time=5),
  tree |> parse_newick() |>
    getInfo(prune=TRUE,obscure=TRUE,newick=TRUE) |>
    getElement("newick") |>
    phylopomp:::treeplot(points=TRUE,t0=0,time=5),
  nrow=1
)

runSEIR(time=5,S0=100,I0=3,E0=5,pop=108) -> x

plot_grid(
  x |> plot(prune=TRUE,obscure=FALSE,points=TRUE),
  x |> lineages(prune=TRUE,obscure=FALSE) |> plot(legend.position="none"),
  x |> newick(prune=TRUE,obscure=FALSE) |>
    parse_newick() |>
    getInfo(prune=TRUE,obscure=FALSE,lineages=TRUE,
      time=TRUE,yaml=TRUE,structure=TRUE) |>
    getElement("lineages") |>
    plot(legend.position="none")+
    expand_limits(x=5),
  align="hv",axis="tblr",
  ncol=1,rel_heights=c(2,1,1)
)

stopifnot(
  all.equal(
    x |> lineages(prune=TRUE,obscure=FALSE) |>
      slice(1:20),
    x |> newick(prune=TRUE,obscure=FALSE) |>
      parse_newick() |>
      getInfo(prune=TRUE,obscure=FALSE,lineages=TRUE) |>
      getElement("lineages") |>
      slice(1:20),
    tolerance=1e-4
  )
)

plot_grid(
  x |> newick(prune=TRUE,obscure=FALSE) |>
    parse_newick(tf=3) |>
    plot()+expand_limits(x=7),
  x |> newick(prune=TRUE,obscure=FALSE) |>
    parse_newick() |>
    plot()+expand_limits(x=7),
  x |> newick(prune=TRUE,obscure=FALSE) |>
    parse_newick(tf=7) |>
    plot()+expand_limits(x=7),
  align="hv",axis="tblr",
  ncol=1,rel_heights=c(1,1,1)
)

try(
  r"{(o_9_1:1.000000,b_1_3:1.000000)m_0_0:0.300000;}" |>
     parse_newick() |>
     newick(extended=FALSE,prune=FALSE)
)

r"{(o_9_1:1.000000,b_1_3:1.000000)m_0_0:0.300000;}" |>
   parse_newick() |>
   newick(extended=FALSE)

r"{(((:0.1)),[&&PhyloPOMP:deme=2|type=extant]:1.00,(((:0.3,:0.1),),):0.3)a:0.5;}" |>
   parse_newick(t0=0.5,tf=2) -> x1
r"{(((:0.1)),chuck[&&PhyloPOMP deme=2]bob[&&PhyloPOMP|type=extant]tim:1.00,(((:0.3,:0.1),),):0.3)a:0.5;}" |>
   parse_newick(t0=0.5,tf=2) -> x2
plot_grid(
  plot(x1,obscure=FALSE,prune=FALSE,points=TRUE),
  plot(x2,obscure=FALSE,prune=FALSE,points=TRUE),
  diagram(x1,obscure=FALSE,prune=FALSE),
  diagram(x2,obscure=FALSE,prune=FALSE)
)

runSEIR(time=5,S0=100,I0=3,E0=5,pop=108) |>
  freeze(seed=39585882) -> x
x |>
  newick(extended=FALSE) |>
  parse_newick() -> y
plot_grid(
  x |> plot(points=TRUE),
  y |> plot(points=TRUE),
  ncol=1
)

try(
  r"{)3:1;(((:0.1)),[&&PhyloPOMP:deme=1]type=sample]:1.00,(((:0.3,:0.1),),):0.3)a:0.5;}" |>
     parse_newick()
)

try(
  r"{yloPOMP:deme=1|type=extant]:1.00,(((:0.3,:0.1),),):0.3)a:0.5;}" |>
     parse_newick()
)

try(
  r"{)3:1;(((:0.1)),[&&PhyloPOMP:deme=1|type=sample]:1.00,(((:0.3,:0.1),),):0.3)a:0.5;}" |>
     parse_newick()
)

try(
  r"{)3:1;(((:0.1)),[&&PhyloPOMP:deme=1|type=extant]:1.00,(((:0.3,:0.1),),):0.3)a:0.5;}" |>
     parse_newick()
)

try(
  r"{((:0.1)),([&&PhyloPOMP:deme=1|type=extant]:1.00,(((:0.3,:0.1),),):0.3)a:0.5;}" |>
     parse_newick()
)

try(
  r"{([&&PhyloPOMP:deme=9|type=bob]:1.0000):0.5;}" |>
     parse_newick()
)

try(
  r"{(()[&&PhyloPOMP:deme=9|type=extant]:1.0000):0.5;}" |>
     parse_newick()
)

try(
  r"{([&&PhyloPOMP|deme=9|type=sample]:1.0000):0.5}" |>
     parse_newick()
)

r"{([&&PhyloPOMP|deme=9|type=sample]:1.0000A):0.5;}" |>
   parse_newick()

r"{([&&NHX:bob=no]:0.2[&&NHX:bob=42],:[&&NHX:bob=maybe]0.1):[&&NHX: bob=yes]0.5;}" |>
   parse_newick() |>
   newick(extended=FALSE)

try(
  r"{(:1,:1):1;((:1,:1):1;}" |>
     parse_newick()
)

stopifnot(
  A=r"{([&&PhyloPOMP deme=2|type=sample]:1.0000):0.5;}" |>
       parse_newick() |>
       getInfo(prune=FALSE,nsample=TRUE) |>
       getElement("nsample")==1,
  B=r"{([&&PhyloPOMP deme=2]:1.0000):0.5;}" |>
       parse_newick() |>
       getInfo(prune=FALSE,nsample=TRUE) |>
       getElement("nsample")==1,
  C=r"{([deme=2|type=sample]:1.0000):0.5;}" |>
       parse_newick() |>
       getInfo(prune=FALSE,nsample=TRUE) |>
       getElement("nsample")==1,
  D=r"{(A[&&PhyloPOMP deme=3]:0.45E-02)B:0.05e+01;}" |>
       parse_newick() |>
       getInfo(time=TRUE) |>
       getElement("time")==0.5045,
  E=r"{(1.0000):0.0;}" |>
       parse_newick() |>
       getInfo(time=TRUE,t0=TRUE,nsample=TRUE) |>
       unlist()==c(t0=0,time=0,nsample=1),
  F=r"{(:1.0000)A;}" |>
       parse_newick() |>
       getInfo(time=TRUE,t0=TRUE,nsample=TRUE) |>
       unlist()==c(t0=0,time=1,nsample=1)
)

try(
  r"{()();}" |>
     parse_newick()
)

try(
  r"{(A();}" |>
     parse_newick()
)

try(
  r"{,();}" |>
     parse_newick()
)

try(
  r"{:2 ,:3 ();}" |>
     parse_newick()
)

try(
  r"{:2 ,3 ();}" |>
     parse_newick()
)

try(
  r"{()[bob =3 jack=[4]:0.3;}" |>
     parse_newick()
)

try(
  "A:3();" |> parse_newick()
)


plot_grid(
  "" |> parse_newick() |> diagram(prune=FALSE,obscure=FALSE),
  ";" |> parse_newick() |> diagram(prune=FALSE,obscure=FALSE),
  "();" |> parse_newick() |> diagram(prune=FALSE,obscure=FALSE),
  "A;" |> parse_newick() |> diagram(prune=FALSE,obscure=FALSE),
  ":1;" |> parse_newick() |> diagram(prune=FALSE,obscure=FALSE),
  ":;" |> parse_newick() |> diagram(prune=FALSE,obscure=FALSE),
  "A:1;" |> parse_newick() |> diagram(prune=FALSE,obscure=FALSE),
  "();" |> parse_newick() |> diagram(prune=FALSE,obscure=FALSE),
  "A3();" |> parse_newick() |> diagram(prune=FALSE,obscure=FALSE),
  "()A:3;" |> parse_newick() |> diagram(prune=FALSE,obscure=FALSE),
  "(:2):2;" |> parse_newick() |> diagram(prune=FALSE,obscure=FALSE),
  r"{([&&PhyloPOMP:deme=9|type=extant]:1.0000):0.5;}" |>
     parse_newick() |> diagram(prune=FALSE,obscure=FALSE),
  nrow=1,labels="AUTO"
)

dev.off()

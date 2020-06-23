suppressPackageStartupMessages({
  library(phylopomp)
  library(tidyverse)
  library(broom)
  library(doParallel)
  library(doRNG)
})

options(digits=3)
png(filename="sirws1-%02d.png",res=100)

theme_set(theme_bw())

registerDoParallel()
registerDoRNG(721604604)

foreach (i=1:50) %dopar% {
  playSIRwS(
    beta=2,
    gamma=1,
    psi=2,
    S0=10000,
    I0=5,
    t0=0,
    times=100,
    tree=FALSE
  ) %>%
    getInfo(tree=FALSE) %>% {
      .$cumhaz %>%
        mutate(p=exp(-Lambda))
    }
} %>%
  bind_rows(.id="rep") -> dat

stopifnot(
  `ties in KS test`=dat %>%
    count(p) %>%
    filter(n>1) %>%
    nrow()==0
)

dat %>%
  do(tidy(ks.test(x=.$p,y=punif))) %>%
  select(p.value)

dat %>%
  group_by(rep) %>%
  do(tidy(ks.test(x=.$p,y=punif))) %>%
  ungroup() %>%
  select(p.value) -> pvals

pvals %>%
  do(tidy(ks.test(x=.$p.value,y=punif))) %>%
  select(p.value) -> ppval

dat %>%
  ggplot(aes(x=p))+
  geom_abline(slope=1)+
  stat_ecdf()+
  annotate("rug",x=pvals$p.value)+
  annotate("text",x=0.2,y=0.8,
    label=sprintf("P==%3.2f",ppval$p.value),parse=TRUE)+
  coord_equal()+
  labs(x=expression(italic(p)),y=expression(italic(F(p))))+
  expand_limits(x=c(0,1),y=c(0,1))

dev.off()

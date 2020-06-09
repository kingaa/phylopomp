library(phylopomp)
library(tidyverse)
library(broom)
library(doParallel)
library(doRNG)

options(digits=3)
png(filename="sirws3-%02d.png",res=100)

theme_set(theme_bw())

registerDoParallel()
registerDoRNG(721604604)

foreach (i=1:500) %dopar% {
  playSIRwS(
    beta=4,
    gamma=1,
    psi=1,
    S0=3,
    I0=1,
    t0=0,
    times=100,
    tree=FALSE
  ) %>% 
    getInfo(tree=FALSE) %>% {
      .$cumhaz %>%
        mutate(p=exp(-Lambda))
    } -> x
} %>%
  bind_rows(.id="rep") -> dat

##dat %>% filter(abs(p-1/2)<1e-8) -> x
##x %>% treeplot(points=T) -> pl

##for (i in seq_along(pl)) {
##  pl[[i]] <- pl[[i]]+geom_vline(xintercept=x %>% slice(i) %>% pull(time))
##}

dat %>%
  filter(!is.na(p)) %>%
  count(p) %>%
  filter(n>1)

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

suppressPackageStartupMessages({
  library(phylopomp)
  library(tidyverse)
  library(broom)
  library(doParallel)
  library(doRNG)
})

options(digits=3)
png(filename="moran2-%02d.png",res=100)

theme_set(theme_bw())

chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
if (nzchar(chk) && chk == "TRUE") {
  ## use 2 cores in CRAN/Travis/AppVeyor
  registerDoParallel(2)
} else {
  ## use all cores in devtools::test()
  registerDoParallel()
}
registerDoRNG(110825570)

foreach (i=1:500) %dopar% {
  playMoran(
    n=100,
    mu=100,
    t0=0,
    stationary=FALSE,
    times=cumsum(rexp(n=100)),
    tree=FALSE
  ) %>%
    getInfo(tree=TRUE) %>% {
      .$cumhaz %>%
        mutate(p=exp(-Lambda))
    }
} %>%
  bind_rows(.id="rep") -> dat

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

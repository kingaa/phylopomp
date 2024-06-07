\dontrun{

  library(ggplot2)
  library(phylopomp)
  times <- seq(from=0,to=8,by=0.1)[-1]

  png_files <- sprintf(
    file.path(tempdir(),"frame%05d.png"),
    seq_len(2*length(times))
  )

  pb <- utils::txtProgressBar(0,2*length(times),0,style=3)
  x <- simulate("SIIR",time=0,Beta1=5,Beta2=10,gamma=1,omega=0.5,
    psi1=0.2,psi2=0.1,sigma12=1,sigma21=1,S0=200,I1_0=3,I2_0=2)

  img <- 1
  for (k in seq.int(from=1,to=length(times),by=1)) {
    x <- simulate(x,time=times[k])
    ggsave(
      filename=png_files[img],
      plot=plot(
        x, t0=0, time=max(times),
        points=FALSE, prune=FALSE, obscure=FALSE,
        palette=c("#ffcb05","#dddddd"),
        axis.line=element_line(color="white"),
        axis.ticks=element_line(color="white"),
        axis.text=element_blank(),
        plot.background=element_rect(fill=NA,color=NA),
        panel.background=element_rect(fill=NA,color=NA)
      ),
      device="png",dpi=300,
      height=2,width=3,units="in"
    )
    setTxtProgressBar(pb,img)
    img <- img+1
  }

  for (k in seq.int(from=length(times),to=1,by=-1)) {
    x <- curtail(x,time=times[k])
    ggsave(
      filename=png_files[img],
      plot=plot(
        x, t0=0, time=max(times),
        points=FALSE, prune=FALSE, obscure=FALSE,
        palette=c("#ffcb05","#dddddd"),
        axis.line=element_line(color="white"),
        axis.ticks=element_line(color="white"),
        axis.text=element_blank(),
        plot.background=element_rect(fill=NA,color=NA),
        panel.background=element_rect(fill=NA,color=NA)
      ),
      device="png",dpi=300,
      height=2,width=3,units="in"
    )
    setTxtProgressBar(pb,img)
    img <- img+1
  }

  library(gifski)
  gif_file <- "movie1.gif"
  gifski(png_files,gif_file,delay=0.08,loop=TRUE)
  unlink(png_files)

}

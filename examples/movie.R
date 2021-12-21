\dontrun{
  library(ggplot2)
  times <- seq(from=0,to=8,by=0.1)[-1]

  png_files <- sprintf(
    file.path(tempdir(),"frame%05d.png"),
    seq_along(times)
  )

  pb <- utils::txtProgressBar(0,length(times),0,style=3)
  x <- simulate("SIIR",time=0,Beta1=5,Beta2=50,gamma=1,
    psi1=0.1,psi2=0,S0=1000,I1_0=10,I2_0=0)
  for (k in seq_len(length(times))) {
    x <- simulate(x,time=times[k])
    ggsave(
      filename=png_files[k],
      plot=plot(
        x, t0=0, time=max(times),
        points=TRUE, prune=FALSE,
        axis.line=element_line(color="white"),
        axis.text=element_text(color="white"),
        plot.background=element_rect(fill=NA,color=NA),
        panel.background=element_rect(fill=NA,color=NA)
      ),
      device="png",dpi=100,
      height=5,width=6,units="in"
    )
    setTxtProgressBar(pb,k)
  }

  library(gifski)
  gif_file <- "movie1.gif"
  gifski(png_files,gif_file,delay=0.02,loop=TRUE)
  unlink(png_files)
}


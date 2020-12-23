##' Genealogy process diagram
##'
##' Produces a diagram of the genealogy process state.
##'
##' @include package.R treeplot.R
##'
##' @param illustration character;
##' illustrations produced by \code{\link{getInfo}} or one of the \code{playX} functions.
##' @param ... graphical parameter settings. See \code{\link[grid:gpar]{gpar}}.
##' 
##' @return A list of \pkg{grid} graphics objects (\code{grob}s), invisibly.
##'
##' @example examples/diagram.R
##'
##' @importFrom grid textGrob circleGrob roundrectGrob linesGrob nullGrob
##' viewport gpar grobTree gList gTree
##' @importFrom dplyr if_else
##' @importFrom readr read_csv
##'
##' @name diagram
##' @rdname diagram
##' 
##' @export
##'
diagram <- function (illustration, ...) {
  dat <- lapply(illustration,read_csv)
  nmax <- max(sapply(dat,nrow)) # longest tableau
  vp <- viewport(height=0.95,width=0.95,gp=gpar(...))
  tg <- lapply(
    dat,
    function (d) {
      tableauGrob(d,n=nmax,vp=vp)
    }
  )
  invisible(tg)
}

tableauGrob <- function (data, n, vp = NULL) {
  gbb <- gList()
  for (k in seq_along(data$player)) {
    gb <- playerGrob(
      name=data$player[k],
      ballA=data$ballA[k],
      ballB=data$ballB[k],
      ballAcol=data$ballAcol[k],
      ballBcol=data$ballBcol[k],
      slate=data$slate[k],
      vp=viewport(x=(k-1/2)/n,width=0.95/n,height=0.95,xscale=c(0,1),yscale=c(0,4))
    )
    gbb <- gList(gbb,gb)
  }
  gTree(
    children=gbb,
    vp=vp
  )
}

playerGrob <- function (name, ballA, ballAcol, ballB, ballBcol, slate, vp = NULL) {
  rr <- roundrectGrob(vp=vp)
  lg <- linesGrob(x=c(0,1),y=25/32,vp=vp)
  nm <- textGrob(label=name,y=7/8,vp=vp)
  if (ballAcol=="g") {
    bt1 <- textGrob(label=ballA,y=5/8,gp=gpar(col="white"),vp=vp)
  } else {
    bt1 <- nullGrob()
  }
  cg1 <- circleGrob(
    y=5/8,r=unit(0.45,"native"),
    gp=gpar(fill=ball_colors[ballAcol],col=ball_colors[ballAcol]),
    vp=vp
  )
  if (ballBcol=="g") {
    bt2 <- textGrob(label=ballB,y=3/8,gp=gpar(col="white"),vp=vp)
  } else {
    bt2 <- nullGrob()
  }
  cg2 <- circleGrob(
    y=3/8,r=unit(0.45,"native"),
    gp=gpar(fill=ball_colors[ballBcol],col=ball_colors[ballBcol]),
    vp=vp
  )
  tt <- textGrob(
    label=if_else(is.finite(slate),as.character(round(slate,1)),"-\u221E"),
    y=1/8,
    gp=gpar(fontface="italic",col="black"),
    vp=vp
  )
  grobTree(
    name=name,
    Frame=rr,
    Div=lg,
    Name=nm,
    Time=tt,
    BallA_ball=cg1,
    BallA_name=bt1,
    BallB_ball=cg2,
    BallB_name=bt2
  )
}
##' Genealogy process diagram
##'
##' Produces a diagram of the genealogy process state.
##'
##' @include package.R treeplot.R
##'
##' @param illus character;
##' illustrations produced by \code{\link{getInfo}} or one of the \code{playX} functions.
##' @param ... graphical parameter settings, suitable for passing to \code{\link[grid:gpar]{gpar}}.
##' 
##' @return A list of \pkg{grid} graphics objects (\code{grob}s), invisibly.
##'
##' @example examples/diagram.R
##'
##' @importFrom dplyr if_else
##' @importFrom readr read_csv
##'
##' @name diagram
##' @rdname diagram
##' 
##' @export
##'
diagram <- function (illus, ...) {
  dat <- lapply(illus,read_csv)
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

##' Diagramming internals
##'
##' Facilities to produce diagrammatic representations
##' of genealogy process states.
##'
##' Code for the resizing text adapted from a blog post by Mark Heckmann
##' (https://ryouready.wordpress.com/2012/08/01/creating-a-text-grob-that-automatically-adjusts-to-viewport-size/).
##' 
##' @rdname internals
##' @keywords internal
##' @param data illustration vector
##' @param n length of longest tableau
##' 
##' @importFrom grid viewport gList gTree
##' @inheritParams grid::grob
##' 
##' @export
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
      vp=viewport(x=(k-1/2)/n,width=0.95/n,height=0.95,
        xscale=c(0,1),yscale=c(0,4))
    )
    gbb <- gList(gbb,gb)
  }
  gTree(
    children=gbb,
    vp=vp
  )
}

##' @rdname internals
##' @keywords internal
##' @param name player name
##' @param ballA,ballB ball names
##' @param ballAcol,ballBcol ball colors
##' @param slate time
##' @importFrom grid roundrectGrob linesGrob textGrob gpar grobTree
##' @inheritParams grid::grob
##' @export
playerGrob <- function (name, ballA, ballAcol, ballB, ballBcol, slate, vp = NULL) {
  rr <- roundrectGrob(vp=vp)
  lg <- linesGrob(x=c(0,1),y=25/32,vp=vp)
  nm <- resizingTextGrob(label=name,y=7/8,vp=vp)
  bgA <- ballGrob(y=5/8,label=ballA,color=ballAcol,vp=vp)
  bgB <- ballGrob(y=3/8,label=ballB,color=ballBcol,vp=vp)
  tt <- resizingTextGrob(
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
    BallA=bgA,
    BallB=bgB
  )
}

##' @rdname internals
##' @keywords internal
##' @include treeplot.R
##' @param ... arguments to be passed to \code{\link[grid:textGrob]{textGrob}}.
##' @importFrom grid grob textGrob viewport
##' @inheritParams grid::textGrob
##' @export
resizingTextGrob <- function (y, ..., vp = NULL) {
  vp1 <- viewport(y=y,width=1,height=1/4)
  tg <- textGrob(...,vp=vp1)
  grob(tg=tg,vp=vp,cl="resizingTextGrob")
}

##' @rdname internals
##' @keywords internal
##' @importFrom grid drawDetails grid.draw
##' @inheritParams grid::drawDetails
##' @method drawDetails resizingTextGrob
##' @export
drawDetails.resizingTextGrob <- function (x, recording = TRUE) {
  grid.draw(x$tg)
}

##' @rdname internals
##' @keywords internal
##' @importFrom grid preDrawDetails convertHeight pushViewport viewport gpar
##' @importFrom scales rescale
##' @inheritParams grid::preDrawDetails
##' @method preDrawDetails resizingTextGrob
##' @export
preDrawDetails.resizingTextGrob <- function (x) {
  h <- convertHeight(unit(1,"snpc"),"pt",valueOnly=TRUE)
  fs <- h/2
  pushViewport(viewport(gp=gpar(fontsize=fs)))
}

##' @rdname internals
##' @keywords internal
##' @importFrom grid postDrawDetails popViewport
##' @inheritParams grid::postDrawDetails
##' @method postDrawDetails resizingTextGrob
##' @export
postDrawDetails.resizingTextGrob <- function (x) {
  popViewport()
}


##' @rdname internals
##' @keywords internal
##' @include treeplot.R
##' @param y height
##' @param label ball name
##' @param color ball color
##' @importFrom grid unit circleGrob textGrob nullGrob grob gpar viewport
##' @inheritParams grid::grob
##' @export
ballGrob <- function (y, label, color, ..., vp = NULL) {
  vp1 <- viewport(y=y,width=1,height=1/4)
  cg <- circleGrob(r=unit(0.48,"native"),
    gp=gpar(fill=ball_colors[color],col=ball_colors[color]),vp=vp1)
  tg <- textGrob(label=label,gp=gpar(col="white"),vp=vp1)
  grob(cg=cg,tg=tg,vp=vp,cl="ballGrob")
}

##' @rdname internals
##' @keywords internal
##' @importFrom grid drawDetails grid.draw
##' @inheritParams grid::drawDetails
##' @method drawDetails ballGrob
##' @export
drawDetails.ballGrob <- function (x, recording = TRUE) {
  grid.draw(x$cg)
  grid.draw(x$tg)
}

##' @rdname internals
##' @keywords internal
##' @importFrom grid preDrawDetails convertHeight pushViewport viewport gpar
##' @importFrom scales rescale
##' @inheritParams grid::preDrawDetails
##' @method preDrawDetails ballGrob
##' @export
preDrawDetails.ballGrob <- function (x) {
  h <- convertHeight(unit(1,"snpc"),"pt",valueOnly=TRUE)
  fs <- h/3
  pushViewport(viewport(gp=gpar(fontsize=fs)))
}

##' @rdname internals
##' @keywords internal
##' @importFrom grid postDrawDetails popViewport
##' @inheritParams grid::postDrawDetails
##' @method postDrawDetails ballGrob
##' @export
postDrawDetails.ballGrob <- function (x) {
  popViewport()
}

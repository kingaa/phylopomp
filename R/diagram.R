##' Genealogy process diagram
##'
##' Produces a diagram of the genealogy process state.
##'
##' @name diagram
##' @include package.R treeplot.R
##'
##' @inheritParams getInfo
##' @inheritParams grid::gTree
##' @param ... graphical parameter settings, suitable for passing to \code{\link[grid:gpar]{gpar}}.
##' 
##' @return A \pkg{grid} graphics object (\code{grob}), invisibly.
##'
##' @example examples/diagram.R
##'
##' @importFrom dplyr if_else
##' @importFrom grid gpar viewport
##'
NULL


##' @rdname diagram
##' @export
diagram <- function (data, ..., vp = NULL) {
  data$structure |>
    genealogyGrob(
      vp=viewport(height=0.95,width=0.95,gp=gpar(...))
    ) |>
    invisible()
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
##' @param data list; genealogy structure
##' @param n length of longest genealogy
##' 
##' @importFrom grid viewport gList gTree
##' @inheritParams grid::grob
##' 
##' @export
genealogyGrob <- function (data, n = length(data$nodes), vp = NULL) {
  gTree(
    children=do.call(
      gList,
      lapply(
        seq_along(data$nodes),
        function (k) {
          nodeGrob(
            data$nodes[[k]],
            vp=viewport(
              x=(k-1/2)/n,
              width=0.95/n,
              height=0.95,
              xscale=c(0,1),
              yscale=c(0,4)
            )
          )
        }
      )
    ),
    vp=vp
  )
}

##' @rdname internals
##' @keywords internal
##' @param data list; node structure
##' @importFrom grid roundrectGrob linesGrob textGrob gpar grobTree
##' @inheritParams grid::grob
##' @export
nodeGrob <- function (data, vp = NULL) {
  gTree(
    name=data$name,
    children=gList(
      roundrectGrob(),
      linesGrob(x=c(0,1),y=25/32),
      resizingTextGrob(
        label=data$name,
        vp=viewport(y=7/8,height=1/4)
      ),
      resizingTextGrob(
        label=if_else(
          is.finite(data$time),
          as.character(round(data$time,1)),
          "-\u221E"
        ),
        vp=viewport(y=1/8,height=1/4),
        gp=gpar(fontface="italic",col="black")
      ),
      pocketGrob(
        data$pocket,
        vp=viewport(y=1/2,height=1/2)
      )
    ),
    vp=vp
  )
}

##' @rdname internals
##' @keywords internal
##' @param data list; pocket structure
##' @importFrom grid roundrectGrob linesGrob textGrob gpar grobTree
##' @inheritParams grid::grob
##' @export
pocketGrob <- function (data, vp = NULL) {
  n <- length(data)
  gTree(
    children=do.call(
      gList,
      lapply(
        seq_along(data),
        function (k) {
          ballGrob(
            data[[k]],
            vp=viewport(
              y=(k-1/2)/n,
              width=1,
              height=0.5/n
            )
          )
        }
      )
    ),
    vp=vp
  )
}

##' @rdname internals
##' @keywords internal
##' @include treeplot.R
##' @importFrom grid unit circleGrob textGrob grob gpar viewport
##' @inheritParams grid::grob
##' @export
ballGrob <- function (data, vp = NULL) {
  grob(
    cg=circleGrob(
      r=unit(0.48,"native"),
      gp=gpar(
        fill=ball_colors[data$color],
        col=ball_colors[data$color]
      )
    ),
    tg=if (data$color %in% c("m","r")) {
         NULL
       } else {
         textGrob(
           label=data$name,
           gp=gpar(col="white")
         )
       },
    vp=vp,
    cl="ballGrob"
  )
}

##' @rdname internals
##' @keywords internal
##' @include treeplot.R
##' @param ... arguments to be passed to \code{\link[grid:textGrob]{textGrob}}.
##' @importFrom grid grob textGrob viewport
##' @inheritParams grid::textGrob
##' @export
resizingTextGrob <- function (..., vp = NULL) {
  grob(
    tg=textGrob(...,vp=viewport(height=1/3)),
    vp=vp,
    cl="resizingTextGrob"
  )
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

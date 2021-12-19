##' Genealogy process diagram
##'
##' Produces a diagram of the genealogy process state.
##'
##' @name diagram
##' @include getinfo.R
##'
##' @inheritParams getInfo
##' @inheritParams grid::gTree
##' @param ... graphical parameter settings, suitable for passing to \code{\link[grid:gpar]{gpar}}.
##' @param m width of plotting window, in nodes.
##' By default, the nodes will be adjusted in width to fit the window.
##' @param n height of the pockets, in balls.
##' By default, the balls will be adjusted in size to fit the space available.
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
diagram <- function (data, prune = TRUE, m = NULL, n = NULL, ..., vp = NULL) {
  data |>
    getInfo(structure=TRUE,prune=prune) |>
    getElement("structure") |>
    genealogyGrob(
      m=m,n=n,
      vp=viewport(height=0.95,width=0.95,gp=gpar(...))
    ) -> x
  class(x) <- c("gpdiag",class(x))
  x
}

##' @rdname internals
##' @keywords internals
##' @method print gpdiag
##' @importFrom grid grid.newpage grid.draw seekViewport pushViewport upViewport
##' @export
print.gpdiag <- function (x, newpage = is.null(vp), vp = NULL, ...) {
  if (newpage) grid.newpage()
  if (is.null(vp)) {
    grid.draw(x)
  } else {
    if (is.character(vp)) {
      seekViewport(vp)
    } else {
      pushViewport(vp)
    }
    grid.draw(x)
    upViewport()
  }
  invisible(x)
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
##' @inheritParams diagram
##' @inheritParams grid::grob
##' 
##' @export
genealogyGrob <- function (data, m = NULL, n = NULL, vp = NULL) {
  if (is.null(m)) m <- length(data$nodes)
  if (is.null(n)) n <- max(sapply(data$nodes,\(node)length(node$pocket)))
  gTree(
    children=do.call(
      gList,
      lapply(
        seq_along(data$nodes),
        function (k) {
          nodeGrob(
            data$nodes[[k]],
            n=n,
            vp=viewport(
              x=(k-1/2)/m,
              width=0.95/m,
              height=0.95
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
##' @param digits non-negative integer;
##' number of decimal digits to print in the node time
##' @importFrom grid roundrectGrob linesGrob textGrob gpar grobTree
##' @inheritParams diagram
##' @inheritParams grid::grob
##' @export
nodeGrob <- function (data, digits = 1, n = NULL, vp = NULL) {
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
          as.character(round(data$time,digits)),
          "-\u221E"
        ),
        vp=viewport(y=1/8,height=1/4),
        gp=gpar(fontface="italic",col="black")
      ),
      pocketGrob(
        data$pocket,
        n=n,
        vp=viewport(y=1/2,height=0.5,width=0.95)
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
pocketGrob <- function (data, n = NULL, vp = NULL) {
  if (is.null(n)) n <- length(data)
  gTree(
    children=do.call(
      gList,
      lapply(
        seq_along(data),
        function (k) {
          ballGrob(
            data[[k]],
            vp=viewport(
              y=(n-k+1/2)/n,
              width=1
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
    tg=if (data$color %in% c("g","o")) {
         textGrob(
           label=data$name,
           gp=gpar(col="white")
         )
       } else {
         NULL
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

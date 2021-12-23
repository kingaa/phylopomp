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
##' @param digits non-negative integer;
##' number of decimal digits to print in the node time
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
diagram <- function (
  object,
  prune = TRUE, obscure = TRUE,
  m = NULL, n = NULL, ..., digits = 1
) {
  object |>
    getInfo(structure=TRUE,prune=prune,obscure=obscure) |>
    getElement("structure") |>
    genealogyGrob(
      m=m,n=n,digits=digits,
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
##' @param object list; genealogy structure
##' @param n length of longest genealogy
##' 
##' @importFrom grid viewport gList gTree
##' @inheritParams diagram
##' @inheritParams grid::grob
##' 
##' @export
genealogyGrob <- function (object, m = NULL, n = NULL, vp = NULL, ...) {
  if (is.null(m)) m <- length(object$nodes)
  if (is.null(n)) {
    object$nodes |>
      lapply(\(node)length(node$pocket)) |>
      as.integer() |>
      c(1L) |>
      max() -> n
  }
  gTree(
    children=do.call(
      gList,
      lapply(
        seq_along(object$nodes),
        function (k) {
          nodeGrob(
            object$nodes[[k]],
            n=n, ...,
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
##' @param object list; node structure
##' @param digits non-negative integer;
##' number of decimal digits to print in the node time
##' @importFrom grid roundrectGrob linesGrob textGrob gpar grobTree
##' @inheritParams diagram
##' @inheritParams grid::grob
##' @export
nodeGrob <- function (object, digits = 1, n = NULL, vp = NULL) {
  gTree(
    name=object$name,
    children=gList(
      roundrectGrob(),
      linesGrob(x=c(0,1),y=25/32),
      resizingTextGrob(
        label=object$name,
        vp=viewport(y=7/8,height=1/4)
      ),
      resizingTextGrob(
        label=if_else(
          is.finite(object$time),
          as.character(round(object$time,digits)),
          "-\u221E"
        ),
        vp=viewport(y=1/8,height=1/4),
        gp=gpar(fontface="italic",col="black")
      ),
      pocketGrob(
        object$pocket,
        n=n,
        vp=viewport(y=1/2,height=0.5,width=0.95)
      )
    ),
    vp=vp
  )
}

##' @rdname internals
##' @keywords internal
##' @param object list; pocket structure
##' @importFrom grid roundrectGrob linesGrob textGrob gpar grobTree
##' @inheritParams grid::grob
##' @export
pocketGrob <- function (object, n = NULL, vp = NULL) {
  if (is.null(n)) n <- length(object)
  y0 <- 1+1/2/n
  dy <- 1/n
  gTree(
    children=do.call(
      gList,
      lapply(
        seq_along(object),
        function (i) {
          ballGrob(
            object[[i]],
            vp=viewport(
              y=y0-i*dy,
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
##' @importFrom grid unit circleGrob textGrob grob gpar viewport
##' @inheritParams grid::grob
##' @export
ballGrob <- function (object, vp = NULL) {
  grob(
    cg=circleGrob(
      r=unit(0.48,"native"),
      gp=gpar(
        fill=ball_colors[object$color],
        col=ball_colors[object$color]
      )
    ),
    tg=if (object$color %in% c("g","o")) {
         textGrob(
           label=object$name,
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

##' @importFrom grid viewport
##' @export
grid::viewport

##' @importFrom cowplot plot_grid
##' @export
cowplot::plot_grid

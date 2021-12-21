##' Fancy tree plotter
##'
##' Plots a genealogical tree.
##'
##' @name plot
##' @include getinfo.R diagram.R
##'
##' @param tree character; tree representation in Newick format.
##' @param t0 numeric; time of the root.
##' @param time numeric; time of the genealogy.
##' @param ladderize Ladderize?
##' @param points Show nodes and tips?
##' @param ... passed to \code{\link[ggplot2]{theme}}.
##' @return A printable \code{ggplot} object.
##'
##' @importFrom ape read.tree
##' @importFrom ggplot2 ggplot expand_limits scale_x_continuous guides fortify scale_color_hue
##' @importFrom ggtree geom_tree geom_nodepoint geom_tippoint theme_tree2
##' @importFrom dplyr mutate left_join count coalesce
##' @importFrom tibble column_to_rownames
##' @importFrom tidyr separate unite expand_grid
##' @importFrom scales alpha
##' @importFrom utils globalVariables
##'
##' @example examples/movie.R
##' 
##' @rdname plot
##' @export
treeplot <- function (tree, time = NULL, t0 = 0,
  ladderize = TRUE, points = FALSE, ...) {

  if (missing(tree) || is.null(tree))
    stop(sQuote("tree")," must be specified.",call.=FALSE)
  t0 <- as.numeric(t0)
  ladderize <- as.logical(ladderize)
  points <- as.logical(points)

  read.tree(text=as.character(tree)) |>
    fortify(ladderize=ladderize) |>
    separate(label,into=c("nodecol","deme","label")) -> dat

  time <- as.numeric(c(time,max(dat$x)))[1L]
  
  if (is.na(t0)) { # root time is to be determined from the current time
    dat |> mutate(x=x-max(x)+time)
  } else {
    dat |> mutate(x=x-min(x)+t0)
  } |>
    mutate(
      vis=nodecol != "i"
    ) -> dat

  ## number of nodes and tips of each color
  expand_grid(
    nodecol=c("o","b","r","g","p","m"),
    isTip=c(TRUE,FALSE)
  ) |>
    left_join(
      dat |> count(nodecol,isTip),
      by=c("nodecol","isTip")
    ) |>
    mutate(
      n=coalesce(n,0),
      isTip=if_else(isTip,"tip","node")
    ) |>
    unite(rowname,c(nodecol,isTip)) |>
    column_to_rownames() |>
    as.matrix() -> ncolors

  attr(dat,"layout") <- "rectangular"

  dat |>
    ggplot(aes(x=x,y=y))+
    geom_tree(aes(alpha=vis,color=deme))+
    expand_limits(x=dat$x)+
    scale_x_continuous()+
    scale_color_hue(l=30,h=c(240,600))+
    scale_alpha_manual(values=c(`TRUE`=1,`FALSE`=0))+
    guides(alpha="none",color="none")+
    expand_limits(x=c(t0,time))+
    theme_tree2()+
    theme(...) -> pl

  if (points) {
    if (ncolors["g_node",] > 0) {
      pl+geom_nodepoint(
           shape=21,fill=ball_colors["g"],color=ball_colors["g"],
           aes(alpha=nodecol=="g")
         ) -> pl
    }
    if (ncolors["b_node",] > 0) {
      pl+geom_nodepoint(
           shape=21,fill=ball_colors["b"],color=ball_colors["b"],
           aes(alpha=nodecol=="b")
         ) -> pl
    }
    if (ncolors["m_node",] > 0) {
      pl+geom_nodepoint(
           shape=21,fill=ball_colors["m"],color=ball_colors["m"],
           aes(alpha=nodecol=="m")
         )->pl
    }
    if (ncolors["p_node",] > 0) {
      pl+geom_nodepoint(
           shape=21,fill=ball_colors["p"],color=ball_colors["p"],
           aes(alpha=nodecol=="p")
         )->pl
    }
    if (ncolors["r_tip",] > 0) {
      pl+geom_tippoint(
           shape=21,fill=ball_colors["r"],color=ball_colors["r"],
           aes(alpha=nodecol=="r")
         ) -> pl
    }
    if (ncolors["b_tip",] > 0) {
      pl+geom_tippoint(
           shape=21,fill=ball_colors["b"],color=ball_colors["b"],
           aes(alpha=nodecol=="b")
         ) -> pl
    }
    if (ncolors["o_tip",] > 0) {
      pl+geom_tippoint(
           shape=21,fill=ball_colors["o"],color=ball_colors["o"],
           aes(alpha=nodecol=="o")
         ) -> pl
    }
    pl+guides(shape="none") -> pl
  }
  pl
}

ball_colors <- c(
  g="darkgreen",
  b="royalblue2",
  r="red2",
  m="saddlebrown",
  o="black",
  p="purple",
  i=alpha("white",0)
)

utils::globalVariables(
         c(".id","k","label","nodecol","deme","vis","x","y","rowname")
       )

##' @inheritParams getInfo
##' @param x object of class \sQuote{gpsim}
##' @param ... passed to \code{\link{treeplot}}
##' @method plot gpsim
##' @rdname plot
##' @export
plot.gpsim <- function (x, ..., time, t0, prune = TRUE, compact = TRUE) {
  out <- getInfo(x,tree=TRUE,t0=TRUE,time=TRUE,prune=prune,compact=compact)
  if (missing(time)) time <- out$time
  if (missing(t0)) t0 <- out$t0
  treeplot(tree=out$tree,time=time,t0=t0,...)
}

##' Fancy tree plotter
##'
##' Plots a genealogical tree.
##'
##' @name treeplot
##' @include getinfo.R
##'
##' @param tree character; tree representation in Newick format.
##' @param root_time numeric; time of the root.
##' @param time numeric; times of the genealogies.
##' @param ladderize Ladderize?
##' @param points Show nodes and tips?
##' @param palette character; color palette to use for branches.
##' See \code{\link[ggplot2]{scale_color_brewer}} for details.
##' @return A printable \code{ggplot} object.
##'
##' @importFrom ape read.tree
##' @importFrom ggplot2 ggplot expand_limits scale_x_continuous guides fortify scale_colour_brewer
##' @importFrom ggtree geom_tree geom_nodepoint geom_tippoint theme_tree2
##' @importFrom dplyr mutate left_join count coalesce
##' @importFrom tibble column_to_rownames
##' @importFrom tidyr separate unite expand_grid
##' @importFrom scales alpha
##' @importFrom utils globalVariables
##'
##' @rdname treeplot
##' @export
treeplot <- function (tree, time = NULL, root_time = 0,
  ladderize = TRUE, points = FALSE, palette = "Set1") {

  if (missing(tree) || is.null(tree))
    stop(sQuote("tree")," must be specified.",call.=FALSE)
  time <- as.numeric(time)
  root_time <- as.numeric(root_time)
  ladderize <- as.logical(ladderize)
  points <- as.logical(points)

  read.tree(text=tree) |>
    fortify(ladderize=ladderize) |>
    separate(label,into=c("nodecol","deme","label")) -> dat
  
  if (is.na(root_time)) { # root time is to be determined from the current time
    dat |>
      mutate(
        x=x-max(x)+time
      )
  } else {
    dat |>
      mutate(
        x=x-min(x)+root_time
      )
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
    scale_colour_brewer(type="qual",palette=palette)+
    scale_alpha_manual(values=c(`TRUE`=1,`FALSE`=0))+
    guides(alpha="none",color="none")+
    theme_tree2() -> pl

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
##' @rdname treeplot
##' @export
plot.gpsim <- function (x, ..., prune = TRUE, compact = TRUE) {
  out <- getInfo(x,tree=TRUE,time=TRUE,prune=prune,compact=compact)
  treeplot(tree=out$tree,time=out$time,...)
}

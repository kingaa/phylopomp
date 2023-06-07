##' Fancy tree plotter
##'
##' Plots a genealogical tree.
##'
##' @name treeplot
##' @include getinfo.R diagram.R
##' @param tree character; tree representation in Newick format.
##' @param t0 numeric; time of the root.
##' @param time numeric; time of the genealogy.
##' @param ladderize Ladderize?
##' @param points Show nodes and tips?
##' @param ... \code{plot} passes extra arguments to \code{\link{treeplot}}.
##' \code{treeplot} passes extra arguments to \code{\link[ggplot2]{theme}}.
##' @return A printable \code{ggplot} object.
##' @example examples/movie.R
##'
NULL

##' @rdname treeplot
##' @inheritParams getInfo
##' @param x object of class \sQuote{gpsim}
##' @method plot gpsim
##' @export
plot.gpsim <- function (
  x, ..., time, t0,
  prune = TRUE, obscure = TRUE
) {
  x |>
    getInfo(
      tree=TRUE,t0=TRUE,time=TRUE,
      prune=prune,
      obscure=obscure
    ) -> out
  if (missing(time)) time <- out$time
  if (missing(t0)) t0 <- out$t0
  treeplot(
    tree=out$tree,
    time=time,
    t0=t0,
    ...
  )
}

##' @rdname treeplot
##' @importFrom ape read.tree
##' @importFrom ggplot2 ggplot expand_limits scale_x_continuous guides fortify
##' @importFrom ggplot2 scale_color_manual scale_alpha_manual
##' @importFrom ggtree geom_tree geom_nodepoint geom_tippoint theme_tree2
##' @importFrom dplyr mutate left_join count coalesce
##' @importFrom tibble column_to_rownames
##' @importFrom tidyr separate unite expand_grid
##' @importFrom scales alpha hue_pal
##' @param palette color palette for branches.
##' This can be furnished either as a function or a vector of colors.
##' If this is a function, it should take a single integer argument, the number of colors required.
##' If it is a vector, it should have at least as many elements as there are demes in the genealogy.
##' @export
treeplot <- function (
  tree, time = NULL, t0 = 0,
  ladderize = TRUE, points = FALSE, ...,
  palette = scales::hue_pal(l=30,h=c(220,580))
) {

  if (missing(tree) || is.null(tree))
    pStop(sQuote("tree")," must be specified.")
  t0 <- as.numeric(t0)
  ladderize <- as.logical(ladderize)
  points <- as.logical(points)

  tree |> as.character() -> tree
  if (nchar(tree)==0L) tree <- "i_NA_NA:0.0;"
  tree |>
    gsub(";$",")i_NA_NA:0.0",x=_) |>
    gsub(";",")i_NA_NA:0.0,(",x=_) -> tree

  paste0(
    "(i_NA_NA:0.0,i_NA_NA:0.0,(",
    tree,
    ")i_NA_NA:0.0;"
  ) |>
  read.tree(text=_) |>
    fortify(ladderize=ladderize) |>
    separate(label,into=c("nodecol","deme","label")) |>
    mutate(
      deme=if_else(deme=="NA",NA_character_,deme),
      label=if_else(label=="NA",NA_character_,label)
    ) -> dat

  ndeme <- max(1L,length(unique(dat$deme))-1L)
  if (is.function(palette)) {
    palette <- palette(ndeme)
  } else {
    if (length(palette) < ndeme)
      pStop(sQuote("palette"),
        " must have length at least ",ndeme,
        " if specified as a vector.")
  }

  time <- as.numeric(c(time,max(dat$x)))[1L]

  if (is.na(t0)) { # root time is to be determined from the current time
    dat |> mutate(x=x-max(x)+time) -> dat
  } else {
    dat |> mutate(x=x-min(x)+t0) -> dat
  }
  dat |> mutate(vis=nodecol != "i") -> dat

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
    scale_color_manual(values=palette,na.value="#ffffff00")+
    scale_alpha_manual(values=c(`TRUE`=1,`FALSE`=0))+
    guides(alpha="none",color="none")+
    expand_limits(x=c(t0,time))+
    theme_tree2()+
    theme(...) -> pl

  if (points) {
    if (ncolors["m_node",] > 0) {
      pl+geom_nodepoint(
           shape=21,fill=ball_colors["m"],color=ball_colors["m"],
           aes(alpha=nodecol=="m")
         ) -> pl
    }
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
  m="saddlebrown",
  o="black",
  i=alpha("white",0)
)

##' @importFrom utils globalVariables
globalVariables(
  c("time","deme","label","x","y","nodecol","isTip","n","rowname","vis")
)

##' Fancy tree plotter
##'
##' Plots a genealogical tree.
##'
##' @name plot
##' @include getinfo.R diagram.R
##' @param ladderize logical; ladderize?
##' @param points logical; show nodes and tips?
##' @param palette color palette for indicating demes.
##' This can be furnished either as a function or a vector of colors.
##' If this is a function, it should take a single integer argument, the number of colors required.
##' If it is a vector, it should have at least as many elements as there are demes in the genealogy.
##' @param ... \code{plot} passes extra arguments to \code{\link[ggplot2]{theme}}.
##' @return A printable \code{ggplot} object.
##' @details
##' Tree-plotting in \pkg{phylopomp} depends on the \pkg{ggtree} package.
##' @references
##' \Yu2017
##' @example examples/seir.R
##' @example examples/lbdp.R
##' @example examples/s2i2r2.R
##' @example examples/movie.R
##'
NULL

##' @rdname plot
##' @inheritParams getInfo
##' @param x object of class \sQuote{gpgen}
##' @importFrom scales hue_pal
##' @method plot gpgen
##' @export
plot.gpgen <- function (
  x, ...,
  prune = TRUE, obscure = TRUE, points = FALSE,
  ladderize = TRUE,
  palette = scales::hue_pal(l=30,h=c(220,580))
) {
  x |>
    getInfo(
      newick=TRUE,t0=TRUE,time=TRUE,
      prune=prune,
      obscure=obscure
    ) -> out
  treeplot(
    tree=out$newick,
    time=out$time,
    t0=out$t0,
    ladderize=ladderize,
    palette=palette,
    points=points
  )+
    theme(...)
}

##' @importFrom ape read.tree
##' @importFrom ggplot2 ggplot expand_limits scale_x_continuous guides fortify
##' @importFrom ggplot2 scale_color_manual scale_alpha_manual
##' @importFrom ggtree geom_tree geom_nodepoint geom_tippoint theme_tree2
##' @importFrom dplyr mutate left_join count coalesce if_else
##' @importFrom tibble column_to_rownames
##' @importFrom tidyr separate_wider_delim unite expand_grid
##' @importFrom scales alpha hue_pal
treeplot <- function (
  tree, time, t0,
  ladderize = TRUE, points = FALSE,
  palette = scales::hue_pal(l=30,h=c(220,580))
) {

  if (missing(tree) || is.null(tree))
    pStop(sQuote("tree")," must be specified.")
  t0 <- as.numeric(t0)
  time <- as.numeric(time)
  ladderize <- as.logical(ladderize)
  points <- as.logical(points)

  tree |> as.character() -> tree
  if (nchar(tree)==0L) tree <- "i_NA_NA:0.0;"
  paste0(
    "(i_NA_NA:0.0,i_NA_NA:0.0,(",
    tree |>
      gsub(";$",")i_NA_NA:0.0",x=_) |>
      gsub(";",")i_NA_NA:0.0,(",x=_) |>
      gsub(r"{\[(&&PhyloPOMP type=(?:sample|extant|node|root))\]}",r"{[\1 deme=0]}",x=_,perl=TRUE) |>
      gsub(r"{\[&&PhyloPOMP (type=\w+) deme=(\d+)\]}",r"{\1_\2_}",x=_,perl=TRUE) |>
      gsub(r"{type=sample}",r"{b}",x=_,perl=TRUE) |>
      gsub(r"{type=extant}",r"{o}",x=_,perl=TRUE) |>
      gsub(r"{type=node}",r"{g}",x=_,perl=TRUE) |>
      gsub(r"{type=root}",r"{m}",x=_,perl=TRUE),
    ")i_NA_NA:0.0;"
  ) |>
    read.tree(text=_) |>
    fortify(ladderize=ladderize) |>
    separate_wider_delim(
      cols=label,
      delim="_",
      names=c("nodecol","deme",NA),
      cols_remove=TRUE
    ) |>
    mutate(
      deme=strtoi(deme),
      vis=nodecol != "i",
      x=x-min(x)+t0,
      y=y-3
    ) -> dat

  demes <- sort(unique(dat$deme))
  palette <- get_palette(palette,demes,undeme="#000000")

  ## number of nodes and tips of each color
  expand_grid(
    nodecol=c("o","b","g","m"),
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
    geom_tree(
      aes(
        alpha=vis,
        color=factor(deme,levels=demes)
      )
    )+
    scale_x_continuous()+
    scale_color_manual(
      values=palette,
      na.value="gray90",
      na.translate=FALSE
    )+
    scale_alpha_manual(values=c(`TRUE`=1,`FALSE`=0))+
    guides(alpha="none",color="none")+
    expand_limits(x=c(dat$x,t0,time))+
    coord_cartesian(ylim=c(0,NA),expand=TRUE,default=FALSE)+
    theme_tree2() -> pl

  if (length(demes) > 1L) {
    pl+
      guides(color=guide_legend(title="deme")) -> pl
  }

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

##' @importFrom utils head
get_palette <- function (palette, demes = NULL, undeme = "#000000") {
  demes <- as.character(demes)
  if (is.function(palette)) {
    if (length(demes) > 0L) {
      palette <- structure(palette(length(demes)),names=demes)
    } else {
      palette <- c()
    }
  } else {
    if (length(palette) < length(demes))
      pStop("if specified as a vector, ",sQuote("palette"),
        " must have length at least ",length(demes),".",who=NULL)
    if (is.null(names(palette))) {
      palette <- structure(head(palette,length(demes)),names=demes)
    } else if (all(demes %in% names(palette))) {
      palette <- palette[demes]
    } else {
      pStop(
        "no palette color assigned for deme ",
        paste(setdiff(demes,names(palette)),sep=","),".",
        who=NULL
      )
    }
  }
  if (all(demes != "0")) palette["0"] <- undeme
  palette
}

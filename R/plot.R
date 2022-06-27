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
##' @param palette color palette for branches.
##' This can be furnished either as a function or a vector of colors.
##' If this is a function, it should take a single integer argument, the number of colors required.
##' If it is a vector, ....
##' @param ... passed to \code{\link[ggplot2]{theme}}.
##' @return A printable \code{ggplot} object.
##'
##' @example examples/movie.R
##'
NULL

##' @inheritParams getInfo
##' @param x object of class \sQuote{gpsim}
##' @param ... passed to \code{\link{treeplot}}
##' @method plot gpsim
##' @rdname plot
##' @importFrom cowplot plot_grid
##' @export
plot.gpsim <- function (
  x, ..., time, t0,
  prune = TRUE, obscure = TRUE, hide = FALSE, compact = TRUE, retimes = FALSE
) {
  out <- getInfo(x,tree=TRUE,t0=TRUE,time=TRUE,
    prune=prune,obscure=obscure,hide=hide,compact=compact,retimes=retimes)
  if (missing(time)) time <- out$time
  if (missing(t0)) t0 <- out$t0
  
  plot_grid(
    plotlist=lapply(out$tree, function(tr) {
      treeplot(tr,time=time,t0=t0,...) -> p
      if (retimes)  p <- p + geom_vline(xintercept=out$retimes, linetype="dashed", alpha=.3)
      p}),
    ncol=1,align="v")
}

##' @rdname plot
##' @importFrom ape read.tree
##' @importFrom ggplot2 ggplot expand_limits scale_x_continuous guides fortify
##' @importFrom ggplot2 scale_color_manual scale_alpha_manual
##' @importFrom scales hue_pal
##' @importFrom ggtree geom_tree geom_nodepoint geom_tippoint theme_tree2
##' @importFrom dplyr mutate left_join count coalesce case_when
##' @importFrom tibble column_to_rownames
##' @importFrom tidyr separate unite expand_grid
##' @importFrom scales alpha
##' @export
treeplot <- function (
    tree, time = NULL, t0 = 0,
    ladderize = TRUE, points = FALSE, ...,
    palette = scales::hue_pal(l=30,h=c(220,580))
) {
  
  if (missing(tree) || is.null(tree))
    stop(sQuote("tree")," must be specified.",call.=FALSE)
  t0 <- as.numeric(t0)
  ladderize <- as.logical(ladderize)
  points <- as.logical(points)
  
  
  read.tree(text=as.character(tree)) |>
    fortify(ladderize=ladderize) |>
    arrange(x) |>
    separate(label,into=c("nodecol","deme","label"), sep="_") |>
    mutate(
      deme=if_else(deme=="NA",NA_character_,deme),
      label=if_else(label=="NA",NA_character_,label),
      time = x,
      # newbs=(table(parent)[as.character(node)]-1) |> as.numeric() |> replace_na(0),  # regarding "node", no, of new branches
      # nodecol=factor(
      #   case_when(
      #     # ((x==0.0) & (!is.na(deme)))~"m",
      #     # ((x>0.0) & (newbs>0))~"g",
      #     # ((x>0.0) & (newbs<1) & (!isTip) & (!grepl("#H", nodecol)))~"b",
      #     # ((abs(x-max(x))>1.5e-6) & (newbs<1) & (isTip))~"r",
      #     # ((abs(x-max(x))<=1.5e-6) & (newbs<1) & (isTip))~"o",
      #     # TRUE~"i",
      #     (grepl("#Ha1", nodecol))~"a1",
      #     (grepl("#Ha2", nodecol))~"a2"
      #   )),
      nodecol=if_else(grepl("#Ha1",nodecol),"a1",if_else(grepl("#Ha2",nodecol),"a2",nodecol))
    ) -> dat
  
  ndeme <- max(1L,length(unique(dat$deme))-1L)
  if (is.function(palette)) {
    palette <- palette(ndeme)
  } else {
    if (length(palette) < ndeme)
      stop("in ",sQuote("treeplot"),": ",sQuote("palette"),
           " must have length at least ",ndeme,
           " if specified as a vector.",call.=FALSE)
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
    nodecol=c("o","b","r","g","p","a1","a2","m"),
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
    theme_tree2() +
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
    if (ncolors["a1_node",] > 0) {
      pl+geom_nodepoint(
        shape=21,fill=ball_colors["a1"],color=ball_colors["a1"],
        aes(alpha=nodecol=="a1")
      )->pl
    }
    if (ncolors["a2_node",] > 0) {
      pl+geom_nodepoint(
        shape=21,fill=ball_colors["a2"],color=ball_colors["a2"],
        aes(alpha=nodecol=="a2")
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
  a1="orange3",
  a2="orange1",
  i=alpha("white",0)
)



##' @importFrom utils globalVariables
globalVariables(
  c(".id","k","label","nodecol","nodecol","deme","vis","x","y","rowname","parent","node","newbs")
)

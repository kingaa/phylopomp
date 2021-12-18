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
##' @importFrom foreach foreach
##' @importFrom ape read.tree
##' @importFrom ggplot2 ggplot expand_limits scale_x_continuous guides fortify scale_colour_brewer
##' @importFrom ggtree geom_tree geom_nodepoint geom_tippoint theme_tree2
##' @importFrom dplyr mutate group_by ungroup
##' @importFrom tidyr separate
##' @importFrom scales alpha
##' @importFrom utils globalVariables
##' 
##'
##' @rdname treeplot
##' @export
treeplot <- function (tree, time = NULL, root_time = 0,
  ladderize = TRUE, points = FALSE, palette = "Set1") {
  if (missing(tree) || is.null(tree))
    stop(sQuote("tree")," must be specified.",call.=FALSE)
  read.tree(text=tree) |>
    fortify(ladderize=ladderize) |>
    separate(label,into=c("nodecol","deme","label")) -> dat
  if (length(tree)==1) dat$.id <- ""
  dat$.id <- as.integer(as.factor(dat$.id))
  if (is.na(root_time)) { # root time is to be determined from the current time
    dat |>
      group_by(.id) |>
      mutate(
        x=x-max(x)+time[.id],
        vis=nodecol != "i"
      ) |>
      ungroup(.id) -> dat
  } else {
    dat |>
      group_by(.id) |>
      mutate(
        x=x-min(x)+root_time,
        vis=nodecol != "i"
      ) |>
      ungroup(.id) -> dat
  }
  
  foreach (
    k=seq_len(length(unique(dat$.id))),
    d=split(dat,dat$.id)
  ) %dopar% {
    attr(d,"layout") <- "rectangular"
    d |>
      ggplot(aes(x=x,y=y))+
      geom_tree(aes(alpha=vis,color=deme))+
      expand_limits(x=dat$x)+
      scale_x_continuous()+
      scale_colour_brewer(type="qual",palette=palette)+
      scale_alpha_manual(values=c(`TRUE`=1,`FALSE`=0))+
      guides(alpha="none",color="none")+
      theme_tree2() -> pl
    if (points) {
      pl+
        geom_nodepoint(
          shape=21,fill=ball_colors["g"],color=ball_colors["g"],
          aes(alpha=nodecol %in% c("g","p"))
        )+
        geom_nodepoint(
          shape=21,fill=ball_colors["b"],color=ball_colors["blue"],
          aes(alpha=nodecol %in% c("b"))
        )+
        geom_nodepoint(
          shape=21,fill=ball_colors["m"],color=ball_colors["m"],
          aes(alpha=nodecol %in% c("m"))
        )+
        geom_tippoint(
          shape=21,fill=ball_colors["r"],color=ball_colors["r"],
          aes(alpha=nodecol %in% c("r"))
        )+
        geom_tippoint(
          shape=21,fill=ball_colors["b"],color=ball_colors["b"],
          aes(alpha=nodecol %in% c("b"))
        )+
        geom_tippoint(
          shape=21,fill=ball_colors["o"],color=ball_colors["o"],
          aes(alpha=nodecol %in% c("o"))
        )+
        scale_shape_manual(
          values=c(i=NA,o=19,g=17,p=17,r=19,b=19)
        )+
        guides(shape="none") -> pl
    }
    pl
  }
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
         c(".id","k","label","nodecol","deme","vis","x","y")
       )

##' @export
plot.gpsim <- function (x, y, ..., prune = TRUE, compact = TRUE) {
  if (!missing(y))
    warning("in ",sQuote("plot.gpsim"),": ",
      sQuote('y')," is ignored.",call.=FALSE)
  out <- getInfo(x,tree=TRUE,time=TRUE,prune=prune,compact=compact)
  treeplot(tree=out$tree,time=out$time,...)
}

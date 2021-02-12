##' Fancy tree plotter
##'
##' Plots a genealogical tree.
##'
##' @include package.R diagram.R
##'
##' @param tree character; tree representation in Newick format.
##' @param illus character; genealogy process diagram information.
##' @param root_time numeric; time of the root.
##' @param time numeric; times of the genealogies.
##' @param ladderize Ladderize?
##' @param points Show nodes and tips?
##' @param diagram Show a diagram?
##'
##' @return A printable \code{ggtree} object.
##'
##' @importFrom foreach foreach
##' @importFrom ape read.tree
##' @importFrom ggplot2 ggplot expand_limits scale_x_continuous scale_color_manual guides fortify
##' @importFrom ggtree geom_tree geom_nodepoint geom_tippoint theme_tree2
##' @importFrom dplyr mutate group_by ungroup
##' @importFrom tidyr separate
##' @importFrom scales alpha
##' @importFrom utils globalVariables
##'
##' @name treeplot
##' @rdname treeplot
##' @export
##' 
treeplot <- function (tree, time = NULL, illus = NULL,
  root_time = 0, ladderize = TRUE, points = FALSE, diagram = FALSE) {
  if (missing(tree) || is.null(tree))
    stop(sQuote("tree")," must be specified.",call.=FALSE)
  read.tree(text=tree) %>%
    fortify(ladderize=ladderize) %>%
    separate(label,into=c("nodecol","label")) -> dat
  if (length(tree)==1) dat$.id <- ""
  dat$.id <- as.integer(as.factor(dat$.id))
  if (is.na(root_time)) { # root time is to be determined from the current time
    dat %>%
      group_by(.id) %>%
      mutate(
        x=x-max(x)+time[.id],
        vis=nodecol != "i",
        nodecol=ball_colors[nodecol]
      ) %>%
      ungroup(.id) -> dat
  } else {
    dat %>%
      group_by(.id) %>%
      mutate(
        x=x-min(x)+root_time,
        vis=nodecol != "i",
        nodecol=ball_colors[nodecol]
      ) %>%
      ungroup(.id) -> dat
  }

  if (diagram) {
    dg <- diagram(illus)
  }
  
  foreach (
    k=seq_len(length(unique(dat$.id))),
    d=split(dat,dat$.id)
  ) %dopar% {
    attr(d,"layout") <- "rectangular"
    d %>%
      ggplot(aes(x=x,y=y))+
      geom_tree(aes(alpha=vis))+
      expand_limits(x=dat$x)+
      scale_x_continuous()+
      scale_alpha_manual(values=c(`TRUE`=1,`FALSE`=0))+
      guides(alpha=FALSE)+
      theme_tree2() -> pl
    if (points) {
      pl+
        geom_nodepoint(aes(color=nodecol))+
        geom_tippoint(aes(color=nodecol))+
        scale_color_identity()+
        guides(color=FALSE) -> pl
    }
    if (diagram) {
      ymin <- 4/3*min(dat$y)-1/3*max(dat$y)
      pl+
        annotation_custom(
          dg[[k]],
          xmin=min(dat$x),xmax=max(dat$x),
          ymin=ymin,ymax=0
        )+
        expand_limits(y=ymin) -> pl
    }
    pl
  }
}

ball_colors <- c(
  g="darkgreen",
  b="royalblue4",
  r="red2",
  n="saddlebrown",
  o="black",
  i=alpha("white",0)
)

utils::globalVariables(
         c(".id","k","label","nodecol","vis","x","y")
       )

##' @export
plot.gpsim <- function (x, y, ...) {
  if (!missing(y))
    warning("in ",sQuote("plot.gpsim"),": ",
      sQuote('y')," is ignored.",call.=FALSE)
  tree <- getElement(x,"tree")
  if (is.null(tree))
    stop("no ",sQuote("tree")," element supplied!",call.=FALSE)
  time <- getElement(x,"time")
  illus <- getElement(x,"illus")
  treeplot(tree=x$tree,time=time,illus=illus,...)
}

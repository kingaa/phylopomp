##' Fancy tree plotter
##'
##' Plots a genealogical tree.
##'
##' @include package.R diagram.R
##'
##' @param data Output of one of the \code{play} functions.
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
##' @importFrom utils globalVariables
##'
##' @name treeplot
##' @rdname treeplot
##' @export
##' 
treeplot <- function (data, ladderize = TRUE, points = FALSE, diagram = FALSE) {
  if (is.null(data$tree))
    stop(sQuote("data")," contains no variable ",sQuote("tree"),call.=FALSE)
  read.tree(text=data$tree) %>%
    fortify(ladderize=ladderize) %>%
    separate(label,into=c("nodecol","label")) -> dat
  if (length(data$tree)==1) dat$.id <- data$time
  dat %>%
    group_by(.id) %>%
    mutate(
      time=as.double(as.character(.id)),
      x=x-max(x)+time,
      vis=nodecol != "i",
      nodecol=ball_colors[nodecol]
    ) %>%
    ungroup(.id) -> dat

  if (diagram) {
    dg <- diagram(data$illustration)
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
  treeplot(x,...)
}

##' Fancy tree plotter
##'
##' Plots a genealogical tree.
##'
##' @include package.R
##'
##' @param data Output of one of the \code{play} functions.
##' @param ladderize Ladderize?
##' @param points Show nodes and tips?
##'
##' @return A printable \code{ggtree} object.
##'
##' @importFrom foreach foreach
##' @importFrom ape read.tree
##' @importFrom ggplot2 ggplot expand_limits scale_x_continuous scale_color_manual guides fortify
##' @importFrom ggtree geom_tree geom_nodepoint geom_tippoint theme_tree2
##' @importFrom dplyr mutate
##' @importFrom tidyr separate
##' @importFrom stringi stri_replace_all_fixed
##' @importFrom utils globalVariables
##'
##' @name treeplot
##' @rdname treeplot
##' @export
##' 
treeplot <- function (data, ladderize = TRUE, points = FALSE) {
  if (is.null(data$tree))
    stop(sQuote("data")," contains no variable ",sQuote("tree"),call.=FALSE)
  ## data$tree %>%
  ##   stri_replace_all_fixed(
  ##     c("inf","nan","-nan"),
  ##     "0.0",
  ##     vectorize_all=FALSE
  ##   ) -> data$tree
  times <- data$time
  if (length(times) != length(data$tree))
    stop("in ",sQuote("treeplot"),", ",sQuote("data")," must have a ",
      sQuote("time")," column.",call.=FALSE)
  foreach (k=seq_along(data$tree)) %dopar% {
    read.tree(text=data$tree[k]) %>%
      fortify(ladderize=ladderize) %>%
      separate(label,into=c("nodecol","label")) %>%
      mutate(
        x=x+times[k]-max(x),
        nodecol=ball_colors[nodecol]
      ) %>%
      ggplot(aes(x=x,y=y))+
      geom_tree(layout="rectangular")+
      expand_limits(x=times)+
      scale_x_continuous()+
      theme_tree2() -> pl
    if (points) {
      pl+
        geom_nodepoint(aes(color=nodecol))+
        geom_tippoint(aes(color=nodecol))+
        scale_color_identity()+
        guides(color=FALSE) -> pl
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

utils::globalVariables(c("label","nodecol","%dopar%","k","x","y"))

##' @export
plot.gpsim <- function (x, y, ...) {
  if (!missing(y))
    warning("in ",sQuote("plot.gpsim"),": ",
      sQuote('y')," is ignored.",call.=FALSE)
  treeplot(x,...)
}

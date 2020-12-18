##' Fancy tree plotter
##'
##' Plots a genealogical tree.
##'
##' @include package.R
##'
##' @param data Output of one of the \code{play} functions.
##' @param times Vector of times.
##' @param ladderize Ladderize?
##' @param points Show nodes and tips?
##'
##' @return A printable \code{ggtree} object.
##'
##' @importFrom foreach foreach
##' @importFrom ape read.tree
##' @importFrom ggplot2 ggplot expand_limits scale_x_continuous scale_color_manual guides fortify
##' @importFrom ggtree geom_tree geom_nodepoint geom_tippoint theme_tree2
##' @importFrom dplyr mutate case_when
##' @importFrom tidyr separate
##' @importFrom stringi stri_replace_all_fixed
##' @importFrom utils globalVariables
##'
##' @name treeplot
##' @rdname treeplot
##' @export
##' 
treeplot <- function (data, times = data$time, ladderize = TRUE, points = FALSE) {
  if (is.null(data$tree))
    stop(sQuote("data")," contains no variable ",sQuote("tree"),call.=FALSE)
  if (is.null(times)) times <- 0
  if (length(times)==1) times <- rep(times,length(data$tree))
  if (length(times)!=length(data$tree))
    stop(sQuote("times")," must have length 1 or equal to ",sQuote("data$tree"),call.=FALSE)
  data$tree %>%
    stri_replace_all_fixed(
      c("inf","nan","-nan"),
      "0.0",
      vectorize_all=FALSE
    ) -> data$tree
  foreach (k=seq_along(data$tree)) %dopar% {
    read.tree(text=data$tree[k]) %>%
      fortify(ladderize=ladderize) %>%
      separate(label,into=c("nodecol","label")) %>%
      mutate(
        nodecol=case_when(
          nodecol=="o"~"black",
          nodecol=="g"~"green",
          nodecol=="b"~"blue",
          nodecol=="r"~"red",
          nodecol=="n"~"brown",
          TRUE~"invisible"
        )
      ) %>%
      ggplot(aes(x=x,y=y))+
      geom_tree(layout="rectangular")+
      expand_limits(x=c(0,times))+
      scale_x_continuous()+
      theme_tree2() -> pl
    if (points) {
      pl+
        geom_nodepoint(aes(color=nodecol))+
        geom_tippoint(aes(color=nodecol))+
        scale_color_manual(
          values=c(
            brown="brown",
            green="darkgreen",
            blue="blue",
            black="black",
            red="red",
            invisible=alpha("black",0)
          )
        )+
        guides(color=FALSE) -> pl
    }
    pl
  }
}

utils::globalVariables(c("case_when","label","nodecol","%dopar%","k","x","y"))

##' @export
plot.gpsim <- function (x, y, ...) {
  if (!missing(y))
    warning("in ",sQuote("plot.gpsim"),": ",
      sQuote('y')," is ignored.",call.=FALSE)
  treeplot(x,...)
}

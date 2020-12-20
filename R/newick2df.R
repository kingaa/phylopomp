##' Convert a tree in Newick format to data frame
##'
##' Convert a genealogical tree in Newick format to a data frame, containing event times, number of lineages, sample indicator, direct-descent indicator, and coalescent indicator.
##'
##' @include package.R
##'
##' @param tree tree data in Newick format.
##'
##' @return A data frame suitable for use as \code{pomp} input.
##' 
##' @example examples/newick2df.R
##'
##' @importFrom ape read.tree
##' @importFrom ggplot2 fortify
##' @importFrom dplyr mutate arrange select
##' @importFrom tidyr replace_na
##' @importFrom utils globalVariables
##'
##' @name newick2df
##' @rdname newick2df
##' @export
##' 
newick2df <- function (tree) {
  if(is.null(tree))
    stop(sQuote("tree")," contains no variable ",call.=FALSE)
  
  read.tree(text=tree) %>%
    fortify(ladderize=TRUE) %>%
    arrange(x) %>%
    mutate(
      sample=as.numeric(isTip & x != 0),
      inter=table(parent)[as.character(node)] %>% replace_na(0),
      dd=as.numeric(inter==1), 
      coal=as.numeric(inter==2 & x!=0),
      lineages=sum(x==0)+cumsum(coal)-cumsum(sample)
    ) %>%
    mutate(
      time=x,
      lineages=lineages-min(lineages)
    ) %>%
    select(time,lineages,dd,coal,sample)
}

utils::globalVariables(c("coal","dd","inter","isTip","lineages",
  "node","parent","time"))

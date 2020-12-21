##' Convert a tree in Newick format to data frame
##'
##' Convert a genealogical tree in Newick format to a data frame, containing event times, number of lineages, sample indicator, direct-descent indicator, and coalescent indicator.
##'
##' @name newick2df
##' @rdname newick2df
##' 
##' @include package.R
##'
##' @param tree tree data in Newick format.
##'
##' @return A data frame suitable for use as \code{pomp} input.
##' This data frame will have three columns:
##' \describe{
##'   \item{time}{numeric}
##'   \item{lineages}{integer; the lineage-count function}
##'   \item{code}{integer; a code describing the nature of the event.
##'   0 indicates a coalescence; 1 indicates a dead sample; 2 indicates a live sample.}
##' }
##' 
##' @example examples/newick2df.R
##'
##' @importFrom ape read.tree
##' @importFrom ggplot2 fortify
##' @importFrom dplyr mutate arrange select filter summarize n group_by ungroup
##' @importFrom tidyr separate
##' @importFrom utils globalVariables
##'
##' @export
##' 
newick2df <- function (tree) {
  if(is.null(tree))
    stop(sQuote("tree")," contains no variable ",call.=FALSE)

  read.tree(text=tree) %>%
    fortify(ladderize=TRUE) %>%
    separate(label,into=c("type","label")) %>%
    select(label,time=x,isTip) %>%
    filter(label!="") %>%
    arrange(time) %>%
    mutate(
      ##      ng=sum(!isTip)-cumsum(!isTip), # no. of branches to right
      ##      nt=sum(isTip)-cumsum(isTip),   # no. of tips to right
      ##      ell=nt-ng                      # no. of lineages
      lineages = sum(isTip) - cumsum(isTip) - sum(!isTip) + cumsum(!isTip)
    ) %>%
    group_by(time) %>%
    summarize(
      lineages=lineages[n()],
      code=1-sum(isTip) # 1 = branch, 0 = dead sample, -1 = live sample
    ) %>%
    ungroup()
}

utils::globalVariables(c("lineages","isTip","time"))
